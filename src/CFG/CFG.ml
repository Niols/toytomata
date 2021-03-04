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

type terminal_and_or_nonterminal =
  | OnlyTerminal of terminal
  | OnlyNonTerminal of nonterminal
  | Both of terminal * nonterminal

let regroup_rhs rhs =
  let (terminals, nonterminals) =
    List.partition (function Terminal _ -> true | _ -> false) rhs
  in
  let terminals = List.map (function Terminal a -> a | _ -> assert false) terminals in
  let nonterminals = List.map (function NonTerminal v -> v | _ -> assert false) nonterminals in
  let nonterminals = List.rev nonterminals in (* actually crucial *)
  let rec regroup group terminals nonterminals =
    match terminals, nonterminals with
    | [], nonterminals -> List.rev_append group (List.map (fun v -> OnlyNonTerminal v) nonterminals)
    | terminals, [] -> List.rev_append group (List.map (fun a -> OnlyTerminal a) terminals)
    | a::terminals, v::nonterminals -> regroup (Both (a, v) :: group) terminals nonterminals
  in
  regroup [] terminals nonterminals

let rec rhs_to_pda pda ?pop ~from_ ~to_ = function
  | [] -> PDA.add_transition pda (from_, None, pop) (to_, None)
  | [one] ->
    (
      match one with
      | OnlyTerminal a -> PDA.add_transition pda (from_, Some a, pop) (to_, None)
      | OnlyNonTerminal v -> PDA.add_transition pda (from_, None, pop) (to_, Some v)
      | Both (a, v) -> PDA.add_transition pda (from_, Some a, pop) (to_, Some v)
    )
  | one :: rhs ->
    (
      let q' = PDA.fresh_state () in
      let pda =
        match one with
        | OnlyTerminal a -> PDA.add_transition pda (from_, Some a, pop) (q', None)
        | OnlyNonTerminal v -> PDA.add_transition pda (from_, None, pop) (q', Some v)
        | Both (a, v) -> PDA.add_transition pda (from_, Some a, pop) (q', Some v)
      in
      rhs_to_pda pda ~from_:q' ~to_ rhs
    )

let to_pda cfg =
  let q0 = PDA.fresh_state () in
  let pda = PDA.make_trivial q0 in
  let q1 = PDA.fresh_state () in
  let pda = PDA.add_final pda q1 in
  let pda = PDA.add_transition pda (q0, None, None) (q1, Some cfg.start) in
  let pda =
    List.fold_left
      (fun pda rule ->
         rhs_to_pda pda ~pop:rule.lhs ~from_:q1 ~to_:q1 (regroup_rhs rule.rhs))
      pda
      cfg.rules
  in
  pda
