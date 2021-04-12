
let () =
  let langs_dir = "languages" in
  Sys.readdir langs_dir
  |> Array.iter
    (fun lang ->
       let lang_dir = Filename.concat langs_dir lang in
       let (complete, words) =
         Filename.concat lang_dir "words"
         |> WordsParser.parse
       in
       Format.eprintf "Language %s has %s %d words@."
         lang
         (match complete with Complete -> "exactly" | Incomplete -> "at least")
         (List.length words);
       List.iter (Format.eprintf "- %s@.") words;
       Format.eprintf "@.")
