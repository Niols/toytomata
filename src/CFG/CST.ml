type 'a located = 'a Common.CSTHelpers.located
[@@deriving show { with_path = false }]

type terminal = string

and nonterminal = string

and component =
  | T of terminal'
  | N of nonterminal'

and production = component' list

and rule =
  | EntryPoints of nonterminal' list
  | Production of nonterminal' * production' list

and cfg = rule' list

and terminal' = terminal located
and nonterminal' = nonterminal located
and component' = component located
and production' = production located
and rule' = rule located
and cfg' = cfg located

[@@deriving show { with_path = false }]
