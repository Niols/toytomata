type terminal_and_or_nonterminal =
  | OnlyTerminal of AST.letter
  | OnlyNonTerminal of AST.symbol
  | Both of AST.letter * AST.symbol

let regroup_production rhs =
  let (ts, ns) =
    List.partition (function CFG.T _ -> true | _ -> false) rhs
  in
  let ts = List.map (function CFG.T a -> a | _ -> assert false) ts in
  let ns = List.map (function CFG.N v -> v | _ -> assert false) ns in
  let ns = List.map CFG.NonTerminal.to_string ns in
  let ns = List.rev ns in (* actually crucial *)
  let rec regroup group ts ns =
    match ts, ns with
    | [], ns -> List.rev_append group (List.map (fun v -> OnlyNonTerminal v) ns)
    | ts, [] -> List.rev_append group (List.map (fun a -> OnlyTerminal a) ts)
    | a::ts, v::ns -> regroup (Both (a, v) :: group) ts ns
  in
  regroup [] ts ns

let rec production_to_pda pda ?pop ~from_ ~to_ = function
  | [] -> AST.add_transition from_ to_ (None, pop, None) pda
  | [one] ->
    (
      match one with
      | OnlyTerminal a -> AST.add_transition from_ to_  (Some a, pop, None) pda
      | OnlyNonTerminal v -> AST.add_transition from_ to_ (None, pop, Some v) pda
      | Both (a, v) -> AST.add_transition from_ to_ (Some a, pop, Some v) pda
    )
  | one :: rhs ->
    (
      let q' = AST.fresh_state () in
      let pda =
        match one with
        | OnlyTerminal a -> AST.add_transition from_ q' (Some a, pop, None) pda
        | OnlyNonTerminal v -> AST.add_transition from_ q' (None, pop, Some v) pda
        | Both (a, v) -> AST.add_transition from_ q' (Some a, pop, Some v) pda
      in
      production_to_pda pda ~from_:q' ~to_ rhs
    )

let cfg_to_pda cfg =
  let cfg = CFG.Transform.eliminate_terminals_after_nonterminal cfg in
  let q0 = AST.fresh_state () in
  let q1 = AST.fresh_state () in
  let pda =
    AST.empty_pda
    |> AST.add_initial q0
    |> AST.add_final q1
  in
  let pda =
    List.fold_left
      (fun pda entrypoint -> AST.add_transition q0 q1 (None, None, Some entrypoint) pda)
      pda
      (CFG.entrypoints cfg |> List.map CFG.NonTerminal.to_string)
  in
  let pda =
    List.fold_left
      (fun pda (v, p) ->
         production_to_pda pda ~pop:(CFG.NonTerminal.to_string v) ~from_:q1 ~to_:q1 (regroup_production p))
      pda
      (CFG.productions_list cfg)
  in
  pda
