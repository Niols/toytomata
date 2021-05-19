open Common

let split_pop_push pda =
  List.fold_left
    (fun pda' (q, q', (a, s, s')) ->
       if s = None || s' = None then
         AST.add_transition q q' (a, s, s') pda'
       else
         let qtmp = AST.fresh_state () in
         let pda' = AST.add_transition q qtmp (a, s, None) pda' in
         let pda' = AST.add_transition qtmp q' (None, None, s') pda' in
         pda')
    AST.empty_pda
    (AST.transitions_list pda)
  |> AST.add_initials (AST.initial_states pda)
  |> AST.add_finals (AST.final_states pda)

let pda_to_cfg pda =
  let nonterminal_of_states_pair =
    let convert = Converter.make_convert (fun _ -> CFG.fresh_nonterminal ()) in
    fun q q' -> convert (q, q')
  in
  let pda = split_pop_push pda in
  let cfg = CFG.empty_cfg in
  let cfg =
    (* For each pair of states p and r: *)
    List.fold_left
      (fun cfg p ->
         List.fold_left
           (fun cfg r ->
              let v_pr = nonterminal_of_states_pair p r in
              if p = r then
                (
                  CFG.add_production v_pr [] cfg
                )
              else
                (
                  (* For each transition p -- a,s/s' -> p' *)
                  List.fold_left
                    (fun cfg (_, p', (a, s, s')) ->
                       match s, s' with
                       | None, None ->
                         (
                           (* Transition without stack *)
                           let v_p'r = nonterminal_of_states_pair p' r in
                           CFG.add_production v_pr
                             ((match a with None -> [] | Some a -> [CFG.T a]) @ [CFG.N v_p'r])
                             cfg
                         )
                       | Some _, None ->
                         (
                           (* We don't handle pop transitions in themselves. *)
                           cfg
                         )
                       | None, Some s' ->
                         (
                           (* For each transition q -- b,X/lambda -> q' *)
                           List.fold_left
                             (fun cfg (q, q', (b, _, _)) ->
                                let v_p'q = nonterminal_of_states_pair p' q in
                                let v_q'r = nonterminal_of_states_pair q' r in
                                CFG.add_production v_pr
                                  ((match a with None -> [] | Some a -> [CFG.T a])
                                   @ [CFG.N v_p'q]
                                   @ (match b with None -> [] | Some b -> [CFG.T b])
                                   @ [CFG.N v_q'r])
                                  cfg
                             )
                             cfg
                             (AST.transitions_list ~that_pop:(Some s') pda)
                         )
                       | Some _, Some _ -> assert false)
                    cfg
                    (AST.transitions_list ~from_:p pda)
                ))
           cfg
           (AST.states pda))
      cfg
      (AST.states pda)
  in
  let cfg =
    List.fold_left
      (fun cfg q ->
         List.fold_left
           (fun cfg q' ->
              let v_qq' = nonterminal_of_states_pair q q' in
              CFG.add_entrypoint v_qq' cfg)
           cfg
           (AST.final_states pda))
      cfg
      (AST.initial_states pda)
  in
  cfg
