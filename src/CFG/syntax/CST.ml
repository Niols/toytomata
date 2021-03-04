type terminal_or_nonterminal =
  | Terminal of string
  | NonTerminal of string
[@@deriving show { with_path = false } ]

type production_case = terminal_or_nonterminal list
[@@deriving show { with_path = false } ]

type rule =
  | Start of string list
  | Production of string * production_case list
[@@deriving show { with_path = false } ]

type grammar = rule list
[@@deriving show { with_path = false } ]
