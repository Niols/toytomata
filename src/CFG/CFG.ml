open AST

(** {2 AST} *)

type t = AST.grammar

(** {2 CST Parsing & Printing} *)

let cst_from_lexbuf = Parser.entrypoint Lexer.read

let cst_from_channel ichan = cst_from_lexbuf (Lexing.from_channel ichan)
let cst_from_string str = cst_from_lexbuf (Lexing.from_string str)

let cst_from_file fname =
  let ichan = open_in fname in
  let grammar = cst_from_channel ichan in
  close_in ichan;
  grammar

let cst_to_string = Format.asprintf "%a" Printer.pp_grammar

let cst_to_channel ochan grammar =
  let fmt = Format.formatter_of_out_channel ochan in
  Printer.pp_grammar fmt grammar

let cst_to_file fname grammar =
  let ochan = open_out fname in
  cst_to_channel ochan grammar;
  close_out ochan

(** {2 AST Parsing & Printing} *)

let from_channel ichan = cst_from_channel ichan |> CST_to_AST.grammar'__to__grammar
let from_string str = cst_from_string str |> CST_to_AST.grammar'__to__grammar
let from_file fname = cst_from_file fname |> CST_to_AST.grammar'__to__grammar

let to_channel ochan g = AST_to_CST.grammar__to__grammar g |> cst_to_channel ochan
let to_string g = AST_to_CST.grammar__to__grammar g |> cst_to_string
let to_file fname g = AST_to_CST.grammar__to__grammar g |> cst_to_file fname

(** {2 Others} *)

let terminals_from_terminal_or_nonterminal = function
  | Terminal a -> [a]
  | _ -> []

let nonterminals_from_terminal_or_nonterminal = function
  | NonTerminal v -> [v]
  | _ -> []

let terminals_from_rule rule =
  List.concat_map terminals_from_terminal_or_nonterminal rule.rhs

let nonterminals_from_rule rule =
  rule.lhs
  :: List.concat_map
    nonterminals_from_terminal_or_nonterminal
    rule.rhs

let terminals_from_grammar grammar =
  List.concat_map terminals_from_rule grammar.rules
  |> List.sort_uniq compare

let nonterminals_from_grammar grammar =
  grammar.start
  :: List.concat_map nonterminals_from_rule grammar.rules
  |> List.sort_uniq compare

(** FIXME: grammar can be inconsistent like nonterminals without rules and all *)

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
  | [] -> PDA.add_transition pda from_ to_ (None, pop, None)
  | [one] ->
    (
      match one with
      | OnlyTerminal a -> PDA.add_transition pda from_ to_  (Some a, pop, None)
      | OnlyNonTerminal v -> PDA.add_transition pda from_ to_ (None, pop, Some v)
      | Both (a, v) -> PDA.add_transition pda from_ to_ (Some a, pop, Some v)
    )
  | one :: rhs ->
    (
      let q' = PDA.fresh_state () in
      let pda =
        match one with
        | OnlyTerminal a -> PDA.add_transition pda from_ to_ (Some a, pop, None)
        | OnlyNonTerminal v -> PDA.add_transition pda from_ to_ (None, pop, Some v)
        | Both (a, v) -> PDA.add_transition pda from_ to_ (Some a, pop, Some v)
      in
      rhs_to_pda pda ~from_:q' ~to_ rhs
    )

let to_pda cfg =
  let q0 = PDA.fresh_state () in
  let pda = PDA.empty_pda in
  let q1 = PDA.fresh_state () in
  let pda = PDA.add_final pda q1 in
  let pda = PDA.add_transition pda q0 q1 (None, None, Some cfg.start) in
  let pda =
    List.fold_left
      (fun pda rule ->
         rhs_to_pda pda ~pop:rule.lhs ~from_:q1 ~to_:q1 (regroup_rhs rule.rhs))
      pda
      cfg.rules
  in
  pda
