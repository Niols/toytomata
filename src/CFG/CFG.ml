module Syntax = CFGSyntax
module AST = AST
module FromSyntax = FromSyntax
module ToSyntax = ToSyntax

open AST

let fresh_nonterminal =
  let counter = ref 0 in
  fun () ->
    incr counter;
    "X" ^ string_of_int !counter

let replace_late_terminals_in_case replacements case =
  let (_, case, replacements) =
    List.fold_left
      (fun (seen_nonterminal, case, replacements) a_or_v ->
         match a_or_v with
         | Terminal a when seen_nonterminal ->
           (
             match List.assoc_opt a replacements with
             | None ->
               let v = fresh_nonterminal () in
               (true, NonTerminal v :: case, (a, v) :: replacements)
             | Some v ->
               (true, NonTerminal v :: case, replacements)
           )
         | Terminal a ->
           (false, Terminal a :: case, replacements)
         | NonTerminal v ->
           (true, NonTerminal v :: case, replacements))
      (false, [], replacements)
      case
  in
  (List.rev case, replacements)

let replace_late_terminals grammar =
  let (rules, replacements) =
    List.fold_left
      (fun (rules, replacements) rule ->
         let (rhs, replacements) = replace_late_terminals_in_case replacements rule.rhs in
         ({ rule with rhs } :: rules, replacements))
      ([], [])
      grammar.rules
  in
  let rules =
    rules @ List.map (fun (a, v) -> { lhs = v; rhs = [Terminal a] }) replacements
  in
  { grammar with rules }

let rec rhs_to_pda pda from_state = function
  | [] -> (pda, from_state)
  | a_or_v :: rhs ->
    match a_or_v with
    | Terminal a ->
      let q' = PDA.fresh_state () in
      let pda = PDA.add_transition pda (from_state, Some a, None) (q', None) in
      rhs_to_pda pda q' rhs
    | NonTerminal v ->
      let q' = PDA.fresh_state () in
      let pda = PDA.add_transition pda (from_state, None, None) (q', Some v) in
      rhs_to_pda pda q' rhs

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
         let (pda, q) = rhs_to_pda pda q rule.rhs in
         let pda = PDA.add_transition pda (q, None, None) (q1, None) in
         pda)
      pda
      cfg.rules
  in
  pda
