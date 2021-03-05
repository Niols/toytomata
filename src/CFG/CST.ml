type 'a located = 'a Common.CSTHelpers.located
[@@deriving show { with_path = false }]

type terminal = string

and nonterminal = string

and terminal_or_nonterminal =
  | Terminal of terminal'
  | NonTerminal of nonterminal'

and production_case = terminal_or_nonterminal' list

and rule =
  | Start of nonterminal' list
  | Production of nonterminal' * production_case' list

and grammar = rule' list

and terminal' = terminal located
and nonterminal' = nonterminal located
and terminal_or_nonterminal' = terminal_or_nonterminal located
and production_case' = production_case located
and rule' = rule located
and grammar' = grammar located

[@@deriving show { with_path = false }]
