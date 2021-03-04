module Syntax = CFGSyntax
module AST = AST
module FromSyntax = FromSyntax

open AST

let fresh_nonterminal =
  let counter = ref 0 in
  fun () -> "X" ^ string_of_int !counter

let replace_late_terminals_in_case case =
  let (_, case, extra_rules) =
    List.fold_left
      (fun (seen_nonterminal, case, extra_rules) a_or_v ->
         match a_or_v with
         | Terminal a when seen_nonterminal ->
           let xa = fresh_nonterminal () in
           (true, NonTerminal xa :: case, {lhs=xa; rhs=[Terminal a]} :: extra_rules)
         | Terminal a ->
           (false, Terminal a :: case, extra_rules)
         | NonTerminal v ->
           (true, NonTerminal v :: case, extra_rules))
      (false, [], [])
      case
  in
  (List.rev case, extra_rules)

let replace_late_terminals grammar =
  let rules =
    List.fold_left
      (fun rules rule ->
         let (rhs, extra_rules) = replace_late_terminals_in_case rule.rhs in
         { rule with rhs } :: (extra_rules @ rules))
      []
      grammar.rules
  in
  { grammar with rules }

let to_pda cfg =
  let q0 = PDA.fresh_state () in
  let pda = PDA.make_trivial q0 in
  let q1 = PDA.fresh_state () in
  let pda = PDA.add_final pda q1 in
  let pda = PDA.add_transition pda (q0, None, None) (q1, Some cfg.start) in
  let pda =
    List.fold_left
      (fun pda rule ->
         let q = PDA.fresh_state () in
         let pda = PDA.add_transition pda (q1, None, Some rule.lhs) (q, None) in
         let (pda, q) =
           List.fold_left
             (fun (pda, q) a_or_v ->
                match a_or_v with
                | Terminal a ->
                  let q' = PDA.fresh_state () in
                  let pda = PDA.add_transition pda (q, Some a, None) (q', None) in
                  (pda, q')
                | NonTerminal v ->
                  let q' = PDA.fresh_state () in
                  let pda = PDA.add_transition pda (q, None, None) (q', Some v) in
                  (pda, q'))
             (pda, q)
             rule.rhs
         in
         let pda = PDA.add_transition pda (q, None, None) (q1, None) in
         pda)
      pda
      cfg.rules
  in
  pda
