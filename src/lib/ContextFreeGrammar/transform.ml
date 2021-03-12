open AST

let start cfg =
  let s0 = fresh_nonterminal ~hint:"S" () in
  empty_cfg
  |> add_entrypoint s0
  |> add_productions s0 (List.map (fun s -> [N s]) (entrypoints cfg))
  |> add_productionss (productions_list cfg)

let term_gen ~always cfg =
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

let term = term_gen ~always:true
let term_right = term_gen ~always:false

let bin cfg =
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

let del _cfg = assert false

let unit _cfg = assert false

let chomsky_normal_form cfg =
  cfg |> start |> term |> bin |> del |> unit

let cnf = chomsky_normal_form
