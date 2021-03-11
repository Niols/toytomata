open CFG
open AST

let replace_late_terminals_in_production replacements p =
  let (_, p, replacements) =
    List.fold_left
      (fun (seen_nonterminal, p, replacements) -> function
         | T a when seen_nonterminal ->
           (
             match List.assoc_opt a replacements with
             | None ->
               let v = fresh_nonterminal () in
               (true, N v :: p, (a, v) :: replacements)
             | Some v ->
               (true, N v :: p, replacements)
           )
         | T a ->
           (false, T a :: p, replacements)
         | N v ->
           (true, N v :: p, replacements))
      (false, [], replacements)
      p
  in
  (List.rev p, replacements)

let replace_late_terminals cfg =
  let (productions, replacements) =
    List.fold_left
      (fun (productions, replacements) (v, p) ->
         let (p, replacements) = replace_late_terminals_in_production replacements p in
         ((v, p) :: productions, replacements))
      ([], [])
      (productions_list cfg)
  in
  let productions =
    productions @ List.map (fun (a, v) -> (v, [T a])) replacements
  in
  List.fold_left
    (fun cfg (v, p) -> add_production v p cfg)
    (empty_cfg |> add_entrypoints (entrypoints cfg))
    productions

type terminal_and_or_nonterminal =
  | OnlyTerminal of PDA.letter
  | OnlyNonTerminal of PDA.symbol
  | Both of PDA.letter * PDA.symbol

let regroup_production rhs =
  let (ts, ns) =
    List.partition (function T _ -> true | _ -> false) rhs
  in
  let ts = List.map (function T a -> a | _ -> assert false) ts in
  let ns = List.map (function N v -> v | _ -> assert false) ns in
  let ns = List.map NonTerminal.to_string ns in
  let ns = List.rev ns in (* actually crucial *)
  let rec regroup group ts ns =
    match ts, ns with
    | [], ns -> List.rev_append group (List.map (fun v -> OnlyNonTerminal v) ns)
    | ts, [] -> List.rev_append group (List.map (fun a -> OnlyTerminal a) ts)
    | a::ts, v::ns -> regroup (Both (a, v) :: group) ts ns
  in
  regroup [] ts ns

let rec production_to_pda pda ?pop ~from_ ~to_ = function
  | [] -> PDA.add_transition from_ to_ (None, pop, None) pda
  | [one] ->
    (
      match one with
      | OnlyTerminal a -> PDA.add_transition from_ to_  (Some a, pop, None) pda
      | OnlyNonTerminal v -> PDA.add_transition from_ to_ (None, pop, Some v) pda
      | Both (a, v) -> PDA.add_transition from_ to_ (Some a, pop, Some v) pda
    )
  | one :: rhs ->
    (
      let q' = PDA.fresh_state () in
      let pda =
        match one with
        | OnlyTerminal a -> PDA.add_transition from_ q' (Some a, pop, None) pda
        | OnlyNonTerminal v -> PDA.add_transition from_ q' (None, pop, Some v) pda
        | Both (a, v) -> PDA.add_transition from_ q' (Some a, pop, Some v) pda
      in
      production_to_pda pda ~from_:q' ~to_ rhs
    )

let cfg_to_pda cfg =
  let cfg = replace_late_terminals cfg in
  let q0 = PDA.fresh_state () in
  let q1 = PDA.fresh_state () in
  let pda =
    PDA.empty_pda
    |> PDA.add_initial q0
    |> PDA.add_final q1
  in
  let pda =
    List.fold_left
      (fun pda entrypoint -> PDA.add_transition q0 q1 (None, None, Some entrypoint) pda)
      pda
      (entrypoints cfg |> List.map NonTerminal.to_string)
  in
  let pda =
    List.fold_left
      (fun pda (v, p) ->
         production_to_pda pda ~pop:(NonTerminal.to_string v) ~from_:q1 ~to_:q1 (regroup_production p))
      pda
      (productions_list cfg)
  in
  pda
