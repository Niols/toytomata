open Ext
open Common

let length_limit = 10

let get_words lang_dir =
  let (complete, words) =
    Filename.concat lang_dir "words"
    |> WordsParser.parse
  in
  Format.eprintf "has %s %d words@\n"
    (match complete with Complete -> "exactly" | Incomplete -> "at least")
    (List.length words);
  (complete, words)

let get_alphabet words =
  let alphabet =
    List.sort_uniq compare (List.flatten words)
  in
  epf "over alphabet %a@\n" (Word.pp_alphabet ", ") alphabet;
  alphabet

let get_filenames lang_dir prefix =
  Sys.readdir lang_dir
  |> Array.to_list
  |> List.filter
    (fun fname ->
       let lp = String.length prefix in
       String.length fname >= lp && String.sub fname 0 lp = prefix)
  |> List.map (Filename.concat lang_dir)

let get_cfgs lang_dir =
  let cfgs =
    get_filenames lang_dir "cfg"
    |> List.map (fun cfg -> (cfg, CFG.from_file_exn cfg))
  in
  Format.eprintf "has %d CFGs@\n" (List.length cfgs);
  cfgs

let get_pdas lang_dir =
  let pdas =
    get_filenames lang_dir "pda"
    |> List.map (fun pda -> (pda, PDA.from_file_exn pda))
  in
  Format.eprintf "has %d PDAs@\n" (List.length pdas);
  pdas

let rec compare_words_sequences sref complete name stest =
  match sref () with
  | Seq.Nil when complete = WordsParser.Incomplete ->
    (
      match stest () with
      | Seq.Nil ->
        epf "fail!@\nThe list is incomplete but %s does not recognise any more words" name
      | Cons (_, _) -> epf "done!"
    )
  | Seq.Nil -> (* complete = WordsParser.Complete *)
    (
      match stest () with
      | Seq.Nil ->
        epf "done!"
      | Cons (wtest, _) ->
        epf "fail!@\nThe list is complete by %s recognises %a"
          name Word.pp wtest;
    )
  | Cons (wref, sref) ->
    (
      match stest () with
      | Seq.Nil ->
        (
          epf "fail!@\n%s does not recognise any more words (up to length %d) but the list is not done (it still contains at least %a)"
          name length_limit Word.pp wref
        )
      | Cons (wtest, stest) ->
        (match Word.compare wref wtest with
         | n when n < 0 ->
           epf "fail!@\n%s does not recognise %a but it is in the list!"
             name Word.pp wref
         | n when n > 0 ->
           epf "fail!@\n%s recognises %a but it is not in the list!"
             name Word.pp wtest
         | _ ->
           compare_words_sequences sref complete name stest)
    )

module type Accepter = sig
  type t

  val key : string

  val alphabet : t -> string list
  val accepts : t -> string list -> bool
end

let check_accepter (type s) words complete alphabet (name, (obj:s)) (module Obj : Accepter with type t = s) =
  epf "@[<h 2>checking %s `%s`... " Obj.key name;
  if List.sort compare (Obj.alphabet obj) <> alphabet then
    epf "fail@\nthe alphabet of %s (%a) does not correspond to the expected one."
      name (Word.pp_alphabet ", ") (Obj.alphabet obj);
  Word.not_all_words ~length_limit alphabet
  |> Seq.filter (fun word -> Obj.accepts obj word)
  (* note that this sequence never ends; in particular, if there are no more
     words recognised by the object, then the filter just hangs forever *)
  |> compare_words_sequences
    (List.to_seq words) complete name;
  epf "@]@\n"

let check_cfg words complete alphabet (name, cfg) =
  check_accepter words complete alphabet (name, cfg) (module CFG)

let check_pda words complete alphabet (name, pda) =
  check_accepter words complete alphabet (name, pda) (module PDA)

let check_language lang lang_dir =
  epf "@[<h 2>Language `%s`:@\n" lang;
  let (complete, words) = get_words lang_dir in
  let alphabet = get_alphabet words in
  let cfgs = get_cfgs lang_dir in
  let pdas = get_pdas lang_dir in
  List.iter (check_cfg words complete alphabet) cfgs;
  List.iter (check_pda words complete alphabet) pdas;
  epf "@]@."

let () =
  let langs_dir = "languages" in
  Array.iter
    (fun lang ->
       let lang_dir = Filename.concat langs_dir lang in
       check_language lang lang_dir)
    (Sys.readdir langs_dir)
