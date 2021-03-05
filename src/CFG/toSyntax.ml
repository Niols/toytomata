open Common
open AST
open CFGSyntax

let terminal_or_nonterminal__to__terminal_or_nonterminal = function
  | Terminal a -> CST.Terminal (STHelper.dummily a)
  | NonTerminal v -> CST.NonTerminal (STHelper.dummily v)

let terminal_or_nonterminal__to__terminal_or_nonterminal' =
  STHelper.map_add_dummy terminal_or_nonterminal__to__terminal_or_nonterminal

let rule__to__rule rule =
  CST.Production (
    STHelper.dummily rule.lhs,
    [STHelper.dummily (List.map terminal_or_nonterminal__to__terminal_or_nonterminal' rule.rhs)]
  )

let grammar__to__grammar grammar =
  CST.Start [STHelper.dummily grammar.start]
  :: List.map rule__to__rule grammar.rules
