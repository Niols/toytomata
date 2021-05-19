open Ext

let pp_word fmt w =
  Format.pp_print_list
    ~pp_sep:(fun _fmt () -> ())
    Format.pp_print_string
    fmt w

let pp_alphabet fmt a =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> fpf fmt ", ")
    Format.pp_print_string
    fmt a

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
  epf "over alphabet %a@\n" pp_alphabet alphabet;
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

let all_words (alphabet: 'a list) : 'a list Seq.t =
  let alphabet = List.to_seq alphabet in
  let next_words (words: 'a list Seq.t) : 'a list Seq.t =
    Seq.flat_map
      (fun word ->
         Seq.map
           (fun letter -> letter :: word)
           alphabet)
      words
  in
  let rec all_words words =
    Seq.Cons (words, fun () -> all_words (next_words words))
  in
  (fun () -> all_words (fun () -> Cons ([], Seq.empty)))
  |> Seq.flatten
  |> Seq.map List.rev

let compare_words w1 w2 =
  let c = compare (List.length w1) (List.length w2) in
  if c <> 0 then c
  else compare w1 w2

let rec compare_words_sequences sref complete name stest =
  match sref (), stest () with
  | Seq.Nil, Seq.Nil -> epf "done!@\n"
  | Nil, _ when complete = WordsParser.Incomplete -> epf "done!@\n"
  | Nil, Cons (w, _) ->
    epf "fail!@\n%s recognises %a but it is not in the list!@\n"
      name pp_word w
  | _, Nil -> assert false (* should never happen because we try all words *)
  | Cons (wref, sref), Cons (wtest, stest) ->
    (match compare_words wref wtest with
     | n when n < 0 ->
       epf "fail!@\n%s does not recognise %a but it is in the list!@\n"
         name pp_word wref
     | n when n > 0 ->
       epf "fail!@\n%s recognises %a but it is not in the list!@\n"
         name pp_word wtest
     | _ ->
       compare_words_sequences sref complete name stest)

module type Accepter = sig
  type t

  val key : string

  val alphabet : t -> string list
  val accepts : t -> string list -> bool
end

let check_accepter (type s) words complete alphabet (name, (obj:s)) (module Obj : Accepter with type t = s) =
  epf "@[<h 2>checking %s `%s`... " Obj.key name;
  if List.sort compare (Obj.alphabet obj) <> alphabet then
    epf "fail@\nthe alphabet of %s (%a) does not correspond to the expected one.@\n"
      name pp_alphabet (Obj.alphabet obj);
  all_words alphabet
  |> Seq.filter (fun word -> Obj.accepts obj word)
  |> compare_words_sequences
    (List.to_seq words) complete name;
  epf "@]"

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
