

let split_pop_push pda =
  let pda' =
    List.fold_left
      (fun pda' ((q, a, s), (q', s')) ->
         if s = None || s' = None then
           PDA.add_transition pda' q q' (a, s, s')
         else
           let qtmp = PDA.fresh_state () in
           let pda' = PDA.add_transition pda' q qtmp (a, s, None) in
           let pda' = PDA.add_transition pda' qtmp q' (None, None, s') in
           pda')
      PDA.empty_pda
      pda.PDA.transitions
  in
  { pda' with
    initials = pda.initials ;
    finals = pda.finals }

let pda_to_cfg pda =
  let nonterminal_of_states_pair =
    let table = Hashtbl.create 8 in
    fun q q' ->
      match Hashtbl.find_opt table (q, q') with
      | Some v -> v
      | None ->
        let v = CFG.fresh_nonterminal () in
        Hashtbl.add table (q, q') v;
        v
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
                    (fun cfg ((_, a, s), (p', s')) ->
                       match s, s' with
                       | None, None ->
                         (
                           (* Transition without stack *)
                           let v_p'r = nonterminal_of_states_pair p' r in
                           CFG.add_production v_pr
                             (match a with
                              | None -> [NonTerminal v_p'r]
                              | Some a -> [Terminal a; NonTerminal v_p'r])
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
                             (fun cfg ((q, b, _), (q', _)) ->
                                let v_p'q = nonterminal_of_states_pair p' q in
                                let v_q'r = nonterminal_of_states_pair q' r in
                                CFG.add_production v_pr
                                  (match a, b with
                                   | None, None -> [NonTerminal v_p'q; NonTerminal v_q'r]
                                   | Some a, None -> [Terminal a; NonTerminal v_p'q; NonTerminal v_q'r]
                                   | None, Some b -> [NonTerminal v_p'q; Terminal b; NonTerminal v_q'r]
                                   | Some a, Some b -> [Terminal a; NonTerminal v_p'q; Terminal b; NonTerminal v_q'r])
                                  cfg
                             )
                             cfg
                             (PDA.transitions ~that_pop:(Some s') pda)
                         )
                       | Some _, Some _ -> assert false)
                    cfg
                    (PDA.transitions ~from_:p pda)
                ))
           cfg
           (PDA.states pda))
      cfg
      (PDA.states pda)
  in
  let cfg =
    List.fold_left
      (fun cfg q ->
         List.fold_left
           (fun cfg q' ->
              let v_qq' = nonterminal_of_states_pair q q' in
              CFG.add_entrypoint v_qq' cfg)
           cfg
           pda.finals)
      cfg
      pda.initials
  in
  cfg
