type terminal = string

and nonterminal = string

and terminal_or_nonterminal =
  | Terminal of terminal
  | NonTerminal of nonterminal

and rule =
  { lhs : nonterminal ;
    rhs : terminal_or_nonterminal list }

and grammar =
  { start : nonterminal ;
    rules : rule list }

[@@deriving show { with_path = false } ]

let terminals_from_terminal_or_nonterminal = function
  | Terminal a -> [a]
  | _ -> []

let nonterminals_from_terminal_or_nonterminal = function
  | NonTerminal v -> [v]
  | _ -> []

let terminals_from_rule rule =
  List.concat_map terminals_from_terminal_or_nonterminal rule.rhs

let nonterminals_from_rule rule =
  rule.lhs
  :: List.concat_map
    nonterminals_from_terminal_or_nonterminal
    rule.rhs

let terminals_from_grammar grammar =
  List.concat_map terminals_from_rule grammar.rules
  |> List.sort_uniq compare

let nonterminals_from_grammar grammar =
  grammar.start
  :: List.concat_map nonterminals_from_rule grammar.rules
  |> List.sort_uniq compare

(** FIXME: grammar can be inconsistent like nonterminals without rules and all *)
