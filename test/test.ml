open Ext

let get_words lang_dir =
  let (complete, words) =
    Filename.concat lang_dir "words"
    |> WordsParser.parse
  in
  Format.eprintf "  has %s %d words@."
    (match complete with Complete -> "exactly" | Incomplete -> "at least")
    (List.length words);
  (complete, words)

let get_alphabet words =
  let alphabet =
    List.sort_uniq compare (List.flatten words)
  in
  epf "  over alphabet %a@."
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> fpf fmt ", ")
       Format.pp_print_string)
    alphabet;
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
    |> List.map (fun cfg -> (cfg, CFG.from_file cfg))
  in
  Format.eprintf "  has %d CFGs@." (List.length cfgs);
  cfgs

let get_pdas lang_dir =
  let pdas =
    get_filenames lang_dir "pda"
    |> List.map (fun pda -> (pda, PDA.from_file pda))
  in
  Format.eprintf "  has %d PDAs@." (List.length pdas);
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
  (fun () -> all_words (Seq.cons [] Seq.empty))
  |> Seq.flatten

let check_language lang lang_dir =
  Format.eprintf "Language `%s`:@." lang;
  let (complete, words) = get_words lang_dir in
  let alphabet = get_alphabet words in
  let cfgs = get_cfgs lang_dir in
  let pdas = get_pdas lang_dir in

  ignore complete;
  ignore words;
  ignore alphabet;
  ignore cfgs;
  ignore pdas;
  assert false

let () =
  let langs_dir = "languages" in
  Array.iter
    (fun lang ->
       let lang_dir = Filename.concat langs_dir lang in
       check_language lang lang_dir)
    (Sys.readdir langs_dir)
