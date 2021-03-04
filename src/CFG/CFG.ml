module Syntax = CFGSyntax
module AST = AST
module FromSyntax = FromSyntax

let fresh_nonterminal =
  let counter = ref 0 in
  fun () -> "X" ^ string_of_int !counter

let replace_late_terminals_in_case case =
  let open AST in
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
  let open AST in
  let rules =
    List.fold_left
      (fun rules rule ->
         let (rhs, extra_rules) = replace_late_terminals_in_case rule.rhs in
         { rule with rhs } :: (extra_rules @ rules))
      []
      grammar.rules
  in
  { grammar with rules }

(* type terminal_or_nonterminal =
 *   | Terminal of terminal
 *   | NonTerminal of nonterminal
 * [@@deriving show { with_path = false } ]
 *
 * type rule =
 *   { lhs : nonterminal ;
 *     rhs : terminal_or_nonterminal list }
 * [@@deriving show { with_path = false } ]
 *
 * type grammar =
 *   { start : nonterminal ;
 *     rules : rule list }
 * [@@deriving show { with_path = false } ]
 *
 *
 * let to_pda cfg =
 *
 *
 * fresh_state
 *
 *     make_trivial
 *
 *     add_final
 *
 *     add_transition *)
