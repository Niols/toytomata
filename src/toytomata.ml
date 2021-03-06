let pf = Format.printf
let stdfmt = Format.std_formatter

let () = pf "Please enter an automaton:@."
let p = PDA.cst_from_channel stdin
let () = pf "@\nYou have entered:@."
let () = PDA.cst_to_channel stdout p

let () = pf "Please enter a first grammar:@."

let g1 = CFG.from_channel stdin
let g1 = CFG.replace_late_terminals g1

(* let () =
 *   pf "@\nAfter replacing terminals that appear after a non-terminal:@.";
 *   g1
 *   |> CFG.ToSyntax.grammar__to__grammar
 *   |> CFG.Syntax.Printer.pp_grammar stdfmt *)

let pda1 = CFG.to_pda g1

(* let () = pf "@\nAST:@\n%a@\n@\nPDA:@\n%a@\n@\n" CFG.AST.pp_grammar g1 PDA.pp pda1 *)

let () = pf "@\nPlease enter a second grammar:@."

let g2 = CFG.from_channel stdin

let g2 = CFG.replace_late_terminals g2

(* let () =
 *   pf "@\nAfter replacing terminals that appear after a non-terminal:@.";
 *   g2
 *   |> CFG.ToSyntax.grammar__to__grammar
 *   |> CFG.Syntax.Printer.pp_grammar stdfmt *)

let pda2 = CFG.to_pda g2

(* let () = pf "@\nAST:@\n%a@\n@\nPDA:@\n%a@\n@\n" CFG.AST.pp_grammar g2 PDA.pp pda2 *)

let alphabet =
  CFG.terminals_from_grammar g1 @ CFG.terminals_from_grammar g2
  |> List.sort_uniq compare

let () = pf "@\nAlphabet has %d letters: %s.@." (List.length alphabet) (String.concat ", " alphabet)

exception NotEquivalent of string

let pp_word fmt = function
  | [] -> Format.pp_print_string fmt "λ"
  | word -> Format.(pp_print_list ~pp_sep:(fun _fmt () -> ()) pp_print_string) fmt word

let test_all_words () =
  let not_equivalent first second word =
    raise (NotEquivalent (Format.(
        asprintf "The %s grammar accepts \"%a\" but the %s does not."
          first pp_word word second
      )))
  in
  let rec test_all_words length words =
    pf "\r[%d] @?" length;
    let next_words =
      List.concat_map
        (fun word ->
           match PDA.accepts pda1 word, PDA.accepts pda2 word with
           | true, true ->
             pf "%a@\n\r[%d] @?" pp_word word length;
             List.map (fun a -> a :: word) alphabet
           | false, false -> List.map (fun a -> a :: word) alphabet
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

let () = pf "@\nTesting the two grammars on all the words.@\nThe output shows all the words accepted by both grammars.@\nThe tool stops as soon as it finds a word that differenciates@\nthe two grammars, and loops forever if there is none.@."
let () = test_all_words ()
