open AST
open CFGSyntax

let terminal_or_nonterminal__to__terminal_or_nonterminal = function
  | Terminal a -> CST.Terminal a
  | NonTerminal v -> CST.NonTerminal v

let rule__to__rule rule =
  CST.Production (rule.lhs, [List.map terminal_or_nonterminal__to__terminal_or_nonterminal rule.rhs])

let grammar__to__grammar grammar =
  CST.Start [grammar.start]
  :: List.map rule__to__rule grammar.rules
