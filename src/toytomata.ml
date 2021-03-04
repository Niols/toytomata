let pf = Format.printf

let () = pf "About to parse grammar...@."
let cst = CFG.Syntax.from_channel stdin
let () = pf "Done@."

let () = pf "About to debug print CST...@."
let () = CFG.Syntax.CST.pp_grammar Format.std_formatter cst
let () = pf "@.Done@."

let () = pf "About to convert to AST... @?"
let ast = CFG.FromSyntax.grammar__from__grammar cst
let () = pf "done@."

let () = pf "About to debug print AST...@."
let () = CFG.AST.pp_grammar Format.std_formatter ast
let () = pf "@.Done@."

let () = pf "About to print terminals and nonterminals...@."
let terminals = CFG.AST.terminals_from_grammar ast
let nonterminals = CFG.AST.nonterminals_from_grammar ast
let () = Format.(pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_print_string) Format.std_formatter terminals
let () = pf "@."
let () = Format.(pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_print_string) Format.std_formatter nonterminals
let () = pf "@.Done@."

let () = pf "Replacing terminals by non-terminals... @?"
let ast' = CFG.replace_late_terminals ast
let () = pf "done@."

let () = pf "About to debug print AST...@."
let () = CFG.AST.pp_grammar Format.std_formatter ast'
let () = pf "@.Done@."

let () = pf "About to print grammar...@."
let () = CFG.Syntax.Printer.pp_grammar Format.std_formatter cst
let () = pf "Done@."
