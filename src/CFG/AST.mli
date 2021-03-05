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

val terminals_from_grammar : grammar -> terminal list
val nonterminals_from_grammar : grammar -> nonterminal list
