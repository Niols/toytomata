let pf = Format.printf
let stdfmt = Format.std_formatter

let () = pf "Please enter a first grammar:@."
let g1 = CFG.Syntax.from_channel stdin
let g1 = CFG.FromSyntax.grammar__from__grammar g1
let g1 = CFG.replace_late_terminals g1
let pda1 = CFG.to_pda g1

let () = pf "@\n%a@\n@\n%a@\n@\n" CFG.AST.pp_grammar g1 PDA.pp pda1

let () = pf "Please enter a second grammar:@."
let g2 = CFG.Syntax.from_channel stdin
let g2 = CFG.FromSyntax.grammar__from__grammar g2
let g2 = CFG.replace_late_terminals g2
let pda2 = CFG.to_pda g2

let () = pf "@\n%a@\n@\n%a@\n@\n" CFG.AST.pp_grammar g2 PDA.pp pda2

let alphabet =
  CFG.AST.terminals_from_grammar g1 @ CFG.AST.terminals_from_grammar g2
  |> List.sort_uniq compare

let () = pf "Alphabet has %d letters: %s.@." (List.length alphabet) (String.concat ", " alphabet)

exception NotEquivalent of string

type word = string list [@@deriving show { with_path = false } ]

let test_all_words () =
  let not_equivalent first second word =
    raise (NotEquivalent (Format.(
        asprintf "The %s grammar accepts \"%a\" but the %s does not."
          first pp_word word second
      )))
  in
  let rec test_all_words length words =
    pf "\rTrying words of length %d... @?" length;
    let next_words =
      List.concat_map
        (fun word ->
           match PDA.accepts pda1 word, PDA.accepts pda2 word with
           | true, true | false, false -> List.map (fun a -> a :: word) alphabet
           | true, false -> not_equivalent "first" "second" word
           | false, true -> not_equivalent "second" "first" word)
        words
    in
    test_all_words (length + 1) next_words
  in
  try
    test_all_words 0 [[]]
  with
    NotEquivalent msg -> pf "@.%s" msg

let () = test_all_words ()
