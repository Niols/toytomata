open Common
open AST

let terminal_or_nonterminal__to__terminal_or_nonterminal = function
  | CST.Terminal a -> Terminal (CSTHelpers.value a)
  | CST.NonTerminal v -> NonTerminal (CSTHelpers.value v)

let terminal_or_nonterminal'__to__terminal_or_nonterminal =
  CSTHelpers.map_ignore_located terminal_or_nonterminal__to__terminal_or_nonterminal

let rule__to__grammar cfg = function
  | CST.EntryPoints vs -> { cfg with entrypoints = (List.map CSTHelpers.value vs) @ cfg.entrypoints }
  | Production (lhs, rhs) ->
    let rules =
      List.map
        (fun production_case ->
           { lhs = CSTHelpers.value lhs ;
             rhs = List.map terminal_or_nonterminal'__to__terminal_or_nonterminal (CSTHelpers.value production_case) })
        rhs
    in
    { cfg with rules = rules @ cfg.rules }

let grammar__to__grammar rules =
  List.fold_left
    (fun cfg rule -> rule__to__grammar cfg (CSTHelpers.value rule))
    empty_cfg
    rules

let grammar'__to__grammar = CSTHelpers.map_ignore_located grammar__to__grammar
