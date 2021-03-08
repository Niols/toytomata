open Format
let fpf = fprintf

open CST

let pp_located = Common.CSTHelpers.pp_ignore_located

let pp_constant c = fun fmt () -> fpf fmt c

let pp_emptyword = pp_constant "λ"
let pp_state = pp_print_string
let pp_letter = pp_print_string
let pp_symbol = pp_print_string

let pp_state' = pp_located pp_state

let pp_letter_or_empty fmt = function
  | None -> pp_emptyword fmt ()
  | Some l -> pp_letter fmt l

let pp_letter_or_empty' = pp_located pp_letter_or_empty

let pp_symbol_or_empty fmt = function
  | None -> pp_emptyword fmt ()
  | Some s -> pp_symbol fmt s

let pp_symbol_or_empty' = pp_located pp_symbol_or_empty

let pp_transition fmt (a, s, s') =
  fpf fmt "%a,%a/%a" pp_letter_or_empty' a pp_symbol_or_empty' s pp_symbol_or_empty' s'

let pp_rule fmt = function
  | Initials qs -> fpf fmt "initials %a" (pp_print_list ~pp_sep:(pp_constant ", ") pp_state') qs
  | Finals qs -> fpf fmt "finals %a" (pp_print_list ~pp_sep:(pp_constant ", ") pp_state') qs
  | Transition (q, q', ts) -> fpf fmt "%a -> %a via %a" pp_state' q pp_state' q' (pp_print_list ~pp_sep:(pp_constant " or ") pp_transition) ts

let pp_rule' = pp_located pp_rule

let pp_pda fmt rules =
    pp_print_list ~pp_sep:(pp_constant ";\n") pp_rule' fmt rules;
    pp_print_string fmt ";\n"

let pp_pda' = pp_located pp_pda
