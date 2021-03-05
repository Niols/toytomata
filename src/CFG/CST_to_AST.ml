open Common
open AST

let terminal_or_nonterminal__to__terminal_or_nonterminal = function
  | CST.Terminal a -> Terminal (STHelper.value a)
  | CST.NonTerminal v -> NonTerminal (STHelper.value v)

let terminal_or_nonterminal'__to__terminal_or_nonterminal =
  STHelper.map_ignore_located terminal_or_nonterminal__to__terminal_or_nonterminal

let rule__to__grammar grammar = function
  | CST.Start [start] -> { grammar with start = STHelper.value start }
  | Start _ -> assert false
  | Production (lhs, rhs) ->
    let rules =
      List.map
        (fun production_case ->
           { lhs = STHelper.value lhs ;
             rhs = List.map terminal_or_nonterminal'__to__terminal_or_nonterminal (STHelper.value production_case) })
        rhs
    in
    { grammar with rules = rules @ grammar.rules }

let grammar__to__grammar rules =
  let start =
    match
      List.fold_left (fun start rule ->
          match STHelper.value rule with
          | CST.Start _ when start <> None -> failwith "CFG.FromSyntax.rule__to__grammar: only one start statement allowed"
          | CST.Start [] -> assert false
          | CST.Start [start] -> Some start
          | CST.Start _ -> failwith "CFG.FromSyntax.rule__to__grammar: only one starting non-terminal allowed"
          | _ -> start)
        None
        rules
    with
    | None -> failwith "CFG.FromSyntax.rule__to__grammar: start statement required" (* FIXME: issue a warning and use first non-terminal *)
    | Some start -> start
  in
  List.fold_left
    (fun grammar rule -> rule__to__grammar grammar (STHelper.value rule))
    { start = STHelper.value start ; rules = [] }
    rules

let grammar'__to__grammar = STHelper.map_ignore_located grammar__to__grammar
