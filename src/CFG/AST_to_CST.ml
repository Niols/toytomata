open Common
open AST

let terminal_or_nonterminal__to__terminal_or_nonterminal = function
  | Terminal a -> CST.Terminal (CSTHelpers.dummily a)
  | NonTerminal v -> CST.NonTerminal (CSTHelpers.dummily v)

let terminal_or_nonterminal__to__terminal_or_nonterminal' =
  CSTHelpers.map_add_dummy terminal_or_nonterminal__to__terminal_or_nonterminal

let rule__to__rule rule =
  CST.Production (
    CSTHelpers.dummily rule.lhs,
    [CSTHelpers.dummily (List.map terminal_or_nonterminal__to__terminal_or_nonterminal' rule.rhs)]
  )

let grammar__to__grammar grammar =
  CST.Start [CSTHelpers.dummily grammar.start]
  :: List.map rule__to__rule grammar.rules
