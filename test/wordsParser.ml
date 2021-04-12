type words_list_kind = Complete | Incomplete

let parse fname =
  let ichan = open_in fname in
  let rec parse lines =
    match input_line ichan with
    | "" -> assert false
    | "ε" | "epsilon" -> (* FIXME: s when is_empty_word s *)
      parse ("" :: lines) (* FIXME: abstract type for words *)
    | "…" | "..." ->
      (
        try
          ignore (input_line ichan);
          assert false
        with
          End_of_file -> (Incomplete, List.rev lines)
      )
    | w -> parse (w :: lines)
    | exception End_of_file -> (Complete, List.rev lines)
  in
  parse []
