open AST

let extract_entrypoint cfg =
  let s0 = fresh_nonterminal ~hint:"S" () in
  empty_cfg
  |> add_entrypoint s0
  |> add_productions s0 (List.map (fun s -> [N s]) (entrypoints cfg))
  |> add_productionss (productions_list cfg)

let gen_eliminate_terminals ~always cfg =
  let replacements = Hashtbl.create 8 in
  let replace a =
    match Hashtbl.find_opt replacements a with
    | Some n -> n
    | None ->
      let na = fresh_nonterminal ~hint:("N"^a) () in
      Hashtbl.add replacements a na;
      na
  in
  cfg
  |> update_productions (fun n p ->
      [(n,
        List.fold_left (fun (seen_nonterminal, p) c ->
           match c with
           | N n -> (true, N n :: p)
           | T a ->
             (seen_nonterminal,
              (if seen_nonterminal then N (replace a) else T a) :: p))
          (always, []) p
        |> snd
        |> List.rev)]
    )
  |> Hashtbl.fold (fun a na -> add_production na [T a]) replacements

let eliminate_terminals = gen_eliminate_terminals ~always:true
let eliminate_terminals_after_nonterminal = gen_eliminate_terminals ~always:false

let limit_to_two_nonterminals cfg =
  cfg
  |> update_productions (fun n p ->
      let nb_nonterminals =
        p
        |> List.filter (function N _ -> true | _ -> false)
        |> List.length
      in
      if nb_nonterminals <= 2 then
        [(n, p)]
      else
        (
          let (_, cur_n, cur_p, rest) =
            List.fold_left
              (fun (seen_nonterminal, cur_n, cur_p, rest) c ->
                 match c with
                 | T a -> (seen_nonterminal, cur_n, T a :: cur_p, rest)
                 | N n ->
                   if not seen_nonterminal then
                     (true, cur_n, N n :: cur_p, rest)
                   else
                     let n' = fresh_nonterminal () in
                     let cur_p = N n' :: cur_p in
                     (true, n', [N n], (cur_n, List.rev cur_p) :: rest))
              (false, n, [], [])
              p
          in
          (cur_n, List.rev cur_p) :: rest
        )
    )

let inline_epsilon_productions cfg =
  let rec nullable_nonterminals known =
    let new_known =
      List.fold_left
        (fun known (n, p) ->
           if List.for_all (function T _ -> false | N n -> NonTerminal.Set.mem n known) p then
             NonTerminal.Set.add n known
           else
             known)
        known
        (productions_list cfg)
    in
    if NonTerminal.Set.equal known new_known then
      known
    else
      nullable_nonterminals new_known
  in
  let nullable_nonterminals = nullable_nonterminals NonTerminal.Set.empty in
  let is_nullable n = NonTerminal.Set.mem n nullable_nonterminals in
  cfg
  |> update_productions
    (fun n p ->
       List.fold_left
         (fun ps c ->
           match c with
           | N n when is_nullable n -> List.map (fun p -> N n :: p) ps @ ps
           | _ -> List.map (fun p -> c :: p) ps)
         [[]] p
       |> List.filter ((<>) [])
       |> List.map (fun p -> (n, List.rev p)))

let merge_unit_productions _cfg = assert false

let remove_unreachable cfg =
  let rec compute_reachable reachable n =
    if NonTerminal.Set.mem n reachable then
      reachable
    else
      cfg
      |> productions ~from_:n
      |> Seq.fold_left
        (fun reachable (_, p) ->
           List.fold_left
             (fun reachable -> function
                | N n -> compute_reachable reachable n
                | _ -> reachable)
             reachable
             p)
        (NonTerminal.Set.add n reachable)
  in
  let reachable =
    List.fold_left
      compute_reachable
      NonTerminal.Set.empty
      (entrypoints cfg)
  in
  update_productions
    (fun n p ->
       if NonTerminal.Set.mem n reachable then
         [(n, p)]
       else
         [])
    cfg

(** {2 Chomsky Normal Form} *)

let start = extract_entrypoint
let term = eliminate_terminals
let bin = limit_to_two_nonterminals
let del = inline_epsilon_productions
let unit = merge_unit_productions

let chomsky_normal_form cfg =
  cfg |> start |> term |> bin |> del |> unit

let cnf = chomsky_normal_form
