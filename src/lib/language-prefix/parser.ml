open Common
open AST

let from_file_exn fname =
  let ichan = open_in fname in
  let word_of_word word =
    String.to_seq word
    |> Seq.map Letter.from_char
    |> Word.from_letters
  in
  let rec parse lines =
    match input_line ichan with
    | "" -> assert false
    | "ε" | "epsilon" -> (* FIXME: s when is_empty_word s *)
      parse (Word.empty :: lines)
    | "…" | "..." ->
      (
        try
          ignore (input_line ichan);
          assert false
        with
          End_of_file ->
          from_word_list ~kind:Incomplete (List.rev lines)
      )
    | w -> parse (word_of_word w :: lines)
    | exception End_of_file ->
      from_word_list ~kind:Complete (List.rev lines)
  in
  parse []

let from_channel_exn _ = assert false
let from_string_exn _ = assert false
