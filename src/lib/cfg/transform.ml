open Common
open AST

let extract_entrypoint cfg =
  let s0 = fresh_nonterminal ~hint:"S" () in
  empty_cfg
  |> add_entrypoint s0
  |> add_productions s0 (List.map (fun s -> [N s]) (entrypoints cfg))
  |> add_productionss (productions_list cfg)

let gen_eliminate_terminals ~always cfg =
  let replacements = Converter.make_converter
      (fun a ->
         let a = Letter.to_string a in
         fresh_nonterminal ~hint:("N" ^ a) ())
  in
  let replace = Converter.convert replacements in
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
  |> Hashtbl.fold (fun a na -> add_production na [T a]) replacements.table

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
  (* For each rule eg. [A -> BCD] where [B] and [D] are nullable, replace by [A
     -> BCD | BC | CD | C]. Remove all the rules of the form [A -> eps]. *)
  let cfg =
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
  in
  (* We add again a rule of the form [S -> eps] for each nullable entrypoint. *)
  List.fold_left
    (fun cfg nt ->
       if is_nullable nt then
         add_production nt [] cfg
       else
         cfg)
    cfg
    (entrypoints cfg)

let merge_unit_cycles cfg =
  (* Kosaraju's algorithm [Aho et al. 1983] for strongly connected components.
     FIXME: such algorithms should be taken from the library OCamlGraph. *)
  let rec visit visited l n =
    if NonTerminal.Set.mem n visited then
      (visited, l)
    else
      let (visited, l) =
        Seq.fold_left
          (fun (visited, l) (_, p) ->
             match p with
             | [N n'] -> visit visited l n'
             | _ -> (visited, l)
          )
          (NonTerminal.Set.add n visited, l)
          (productions ~from_:n cfg)
      in
      (visited, n :: l)
  in
  let (_, l) =
    Seq.fold_left
      (fun (visited, l) n ->
         visit visited l n)
      (NonTerminal.Set.empty, [])
      (nonterminals cfg)
  in
  let replacement = Hashtbl.create 8 in
  let rec assign n root =
    match Hashtbl.find_opt replacement n with
    | Some _ -> ()
    | None ->
      Hashtbl.add replacement n root;
      Seq.iter
        (fun (n', p) ->
           match p with
           | [N n0] when NonTerminal.equal n n0 -> assign n' root
           | _ -> ())
        (productions cfg)
  in
  List.iter (fun n -> assign n n) l;
  (* This gives us a hashtable [replacement] in which all nonterminals point to
     the representant of their component. We can then use this table to rewrite
     the grammar. *)
  let replace n = Hashtbl.find replacement n in
  let cfg' =
    empty_cfg
    |> add_entrypoints (entrypoints cfg |> List.map replace)
  in
  Seq.fold_left
    (fun cfg' (n, p) ->
       let n = replace n in
       let p = List.map (function T a -> T a | N n' -> N (replace n')) p in
       match p with
       | [N n'] when NonTerminal.equal n n' -> cfg'
       | _ -> add_production n p cfg')
    cfg'
    (productions cfg)

let inline_unit_productions cfg =
  (* make the graph induced by unit productions acyclic. it simplifies the
     topological exploration for later *)
  let cfg = merge_unit_cycles cfg in

  (* split the CFG into a CFG with all the non-unit production rules and a list
     of the unit production rules *)
  let unit_prods = Hashtbl.create 8 in
  let cfg =
    Seq.fold_left
      (fun cfg (nt, prod) ->
         match prod with
         | [N nt'] -> Hashtbl.add unit_prods nt nt'; cfg
         | _ -> add_production nt prod cfg)
      (add_entrypoints (entrypoints cfg) empty_cfg)
      (productions cfg)
  in

  (* sort the non-terminals in reverse topological order with respect to the
     graph induced by the unit production rules. store this as a list of pairs
     containing the non-terminals and all the non-terminals they lead to via
     unit production rule: (nonterminal * nonterminal list) list *)

  let (_, topo_sort) =
    let rec visit (visited, result) nt =
      if NonTerminal.Set.mem nt visited then
        (visited, result)
      else
        let nts = Hashtbl.find_all unit_prods nt in
        let visited = NonTerminal.Set.add nt visited in
        let (visited, result) = List.fold_left visit (visited, result) nts in
        (visited, (nt, nts) :: result)
    in
    Hashtbl.to_seq_keys unit_prods
    |> Seq.fold_left visit (NonTerminal.Set.empty, [])
  in
  let topo_sort = List.rev topo_sort in

  (* iterate on the non-terminals in reverse topological order. for each unit
     production [A -> B], take all the rules [B -> ...] and add the rules [A ->
     ...] to the CFG. *)
  List.fold_left
    (fun cfg (nt, nts) ->
       List.fold_left
         (fun cfg nt' ->
            (* focus: unit production [nt -> nt'] *)
            add_productions nt (List.map snd (productions_list ~from_:nt' cfg)) cfg
         )
         cfg
         nts)
    cfg
    topo_sort

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
let unit = inline_unit_productions

let chomsky_normal_form cfg =
  cfg |> start |> term |> bin |> del |> unit

let cnf = chomsky_normal_form
