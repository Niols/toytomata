type terminal = string

and nonterminal = string

and terminal_or_nonterminal =
  | Terminal of terminal
  | NonTerminal of nonterminal

and rule =
  { lhs : nonterminal ;
    rhs : terminal_or_nonterminal list }

and cfg =
  { entrypoints : nonterminal list ;
    rules : rule list }

[@@deriving show { with_path = false } ]

let empty_cfg =
  { entrypoints = []; rules = [] }

let add_entrypoint v cfg =
  { cfg with entrypoints = v :: cfg.entrypoints }

let add_entrypoints vs cfg =
  { cfg with entrypoints = vs @ cfg.entrypoints }

let add_production v p cfg =
  { cfg with rules = { lhs = v; rhs = p } :: cfg.rules }
