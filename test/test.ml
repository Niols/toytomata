open Ext
open Common

let length_limit = 12

let pp_done fmt () =
  fpf fmt "\027[1;32mdone!\027[m"

let pp_fail fmt () =
  fpf fmt "\027[1;31mfail!\027[m"

let pp_hl pp fmt a =
  fpf fmt "\027[1;33m%a\027[m" pp a

let pp_hl_str fmt s =
  pp_hl Format.pp_print_string fmt s

let failure = ref false
let fail () =
  epf "%a" pp_fail ();
  failure := true

let get_words lang_dir =
  let lang =
    Filename.concat lang_dir "words"
    |> LanguagePrefix.from_file_exn
  in
  Format.eprintf "has %s %d words@\n"
    (if LanguagePrefix.is_complete lang then "exactly" else "at least")
    (LanguagePrefix.length lang);
  lang

let get_alphabet words =
  let alphabet = Word.list_to_alphabet words in
  epf "over alphabet %a@\n" (Alphabet.pp ", ") alphabet;
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
  | Seq.Nil when not complete ->
    (
      match stest () with
      | Seq.Nil ->
        fail ();
        epf "@\nThe list is incomplete but %a does not recognise any more words"
          pp_hl_str name
      | Cons (_, _) -> epf "%a" pp_done ()
    )
  | Seq.Nil -> (* complete = WordsParser.Complete *)
    (
      match stest () with
      | Seq.Nil ->
        epf "%a" pp_done ()
      | Cons (wtest, _) ->
        fail ();
        epf "@\nThe list is complete but %a recognises %a"
          pp_hl_str name Word.pp wtest;
    )
  | Cons (wref, sref) ->
    (
      match stest () with
      | Seq.Nil ->
        (
          fail ();
          epf "@\n%a does not recognise any more words (up to length %d)@\nbut the list is not done (it still contains at least %a)"
            pp_hl_str name length_limit Word.pp wref
        )
      | Cons (wtest, stest) ->
        (match Word.compare wref wtest with
         | n when n < 0 ->
           fail ();
           epf "@\n%a does not recognise %a but it is in the list!"
             pp_hl_str name Word.pp wref
         | n when n > 0 ->
           fail ();
           epf "@\n%a recognises %a but it is not in the list!"
             pp_hl_str name Word.pp wtest
         | _ ->
           compare_words_sequences sref complete name stest)
    )

module type Accepter = sig
  type t

  val key : string

  val alphabet : t -> Alphabet.t
  val accepts : t -> Word.t -> bool
end

let check_accepter (type s) words complete alphabet (name, (obj:s)) (module Obj : Accepter with type t = s) =
  epf "@[<h 2>checking %s `%a`... " Obj.key pp_hl_str name;
  if not (Alphabet.equal (Obj.alphabet obj) alphabet) then
    (
      fail ();
      epf "@\nthe alphabet of %a (%a) does not correspond to the expected one."
        pp_hl_str name (Alphabet.pp ", ") (Obj.alphabet obj)
    );
  Word.not_all_words ~length_limit alphabet
  |> Seq.filter (Obj.accepts obj) (* much faster than (fun word -> Obj.accepts obj word) *)
  (* note that this sequence never ends; in particular, if there are no more
     words recognised by the object, then the filter just hangs forever *)
  |> compare_words_sequences words complete name;
  epf "@]@\n"

let check_cfg words complete alphabet (name, cfg) =
  check_accepter words complete alphabet (name, cfg) (module CFG)

let check_pda words complete alphabet (name, pda) =
  check_accepter words complete alphabet (name, pda) (module PDA)

let check_language lang lang_dir =
  epf "@[<h 2>Language `%a`:@\n" pp_hl_str lang;
  let lang = get_words lang_dir in
  let alphabet = LanguagePrefix.alphabet lang in
  let words = LanguagePrefix.words lang in
  let complete = LanguagePrefix.is_complete lang in
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

let () = exit (if !failure then 1 else 0)
