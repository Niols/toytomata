type terminal = string
[@@deriving show { with_path = false } ]

type nonterminal = string
[@@deriving show { with_path = false } ]

type terminal_or_nonterminal =
  | Terminal of terminal
  | NonTerminal of nonterminal
[@@deriving show { with_path = false } ]

type rule =
  { lhs : nonterminal ;
    rhs : terminal_or_nonterminal list }
[@@deriving show { with_path = false } ]

type grammar =
  { start : nonterminal ;
    rules : rule list }
[@@deriving show { with_path = false } ]

val terminals_from_grammar : grammar -> terminal list
val nonterminals_from_grammar : grammar -> nonterminal list
