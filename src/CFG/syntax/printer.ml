open Format
let fpf = fprintf

open CST

let pp_constant c = fun fmt () -> fpf fmt c

let pp_emptyword = pp_constant "λ"
let pp_terminal = pp_print_string
let pp_nonterminal = pp_print_string

let pp_terminal_or_nonterminal fmt = function
  | Terminal t -> pp_terminal fmt t
  | NonTerminal v -> pp_nonterminal fmt v

let pp_production_case fmt = function
  | [] -> pp_emptyword fmt ()
  | case ->
    pp_print_list ~pp_sep:(pp_constant " ") pp_terminal_or_nonterminal fmt case

let pp_rule fmt = function
  | Start vs ->
    fpf fmt "start %a"
      (pp_print_list ~pp_sep:(pp_constant ", ") pp_nonterminal) vs
  | Production (v, cases) ->
    fpf fmt "%a -> %a"
      pp_nonterminal v
      (pp_print_list ~pp_sep:(pp_constant " | ") pp_production_case) cases

let pp_grammar fmt grammar =
  pp_print_list ~pp_sep:(pp_constant ";\n") pp_rule fmt grammar;
  pp_print_string fmt ";\n"
