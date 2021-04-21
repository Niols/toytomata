open Format
let fpf = fprintf

open CST

let pp_located = Common.CSTHelpers.pp_ignore_located

let pp_constant c = fun fmt () -> fpf fmt c

let pp_emptyword = pp_constant "λ"
let pp_terminal = pp_print_string
let pp_nonterminal = pp_print_string

let pp_component fmt = function
  | T t -> pp_located pp_terminal fmt t
  | N v -> pp_located pp_nonterminal fmt v

let pp_production fmt = function
  | [] -> pp_emptyword fmt ()
  | case ->
    pp_print_list
      ~pp_sep:(pp_constant " ")
      (pp_located pp_component)
      fmt case

let pp_rule fmt = function
  | EntryPoints vs ->
    fpf fmt "entrypoints %a"
      (pp_print_list ~pp_sep:(pp_constant ", ") (pp_located pp_nonterminal))
      vs

  | Production (v, cases) ->
    fpf fmt "%a -> %a"
      (pp_located pp_nonterminal) v
      (pp_print_list ~pp_sep:(pp_constant " | ") (pp_located pp_production)) cases

let pp_cfg fmt cfg =
  pp_print_list ~pp_sep:(pp_constant ";\n") pp_rule fmt cfg;
  pp_print_string fmt ";\n"

let pp_cfg' = pp_located pp_cfg
