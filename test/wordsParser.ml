type words_list_kind = Complete | Incomplete

let parse fname =
  let ichan = open_in fname in
  let word_of_word word =
    String.to_seq word
    |> Seq.map (String.make 1)
    |> List.of_seq
in
  let rec parse lines =
    match input_line ichan with
    | "" -> assert false
    | "ε" | "epsilon" -> (* FIXME: s when is_empty_word s *)
      parse ([] :: lines)
    | "…" | "..." ->
      (
        try
          ignore (input_line ichan);
          assert false
        with
          End_of_file -> (Incomplete, List.rev lines)
      )
    | w -> parse (word_of_word w :: lines)
    | exception End_of_file -> (Complete, List.rev lines)
  in
  parse []
