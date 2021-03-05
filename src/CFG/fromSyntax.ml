open Common
open AST
open CFGSyntax

let terminal_or_nonterminal__from__terminal_or_nonterminal = function
  | CST.Terminal a -> Terminal (STHelper.value a)
  | CST.NonTerminal v -> NonTerminal (STHelper.value v)

let terminal_or_nonterminal__from__terminal_or_nonterminal' =
  STHelper.map_ignore_located terminal_or_nonterminal__from__terminal_or_nonterminal

let grammar__from__rule grammar = function
  | CST.Start [start] -> { grammar with start = STHelper.value start }
  | Start _ -> assert false
  | Production (lhs, rhs) ->
    let rules =
      List.map
        (fun production_case ->
           { lhs = STHelper.value lhs ;
             rhs = List.map terminal_or_nonterminal__from__terminal_or_nonterminal' (STHelper.value production_case) })
        rhs
    in
    { grammar with rules = rules @ grammar.rules }

let grammar__from__grammar rules =
  let start =
    match
      List.fold_left (fun start rule ->
          match STHelper.value rule with
          | CST.Start _ when start <> None -> failwith "CFG.FromSyntax.grammar__from__rule: only one start statement allowed"
          | CST.Start [] -> assert false
          | CST.Start [start] -> Some start
          | CST.Start _ -> failwith "CFG.FromSyntax.grammar__from__rule: only one starting non-terminal allowed"
          | _ -> start)
        None
        rules
    with
    | None -> failwith "CFG.FromSyntax.grammar__from__rule: start statement required" (* FIXME: issue a warning and use first non-terminal *)
    | Some start -> start
  in
  List.fold_left
    (fun grammar rule -> grammar__from__rule grammar (STHelper.value rule))
    { start = STHelper.value start ; rules = [] }
    rules

let grammar__from__grammar' = STHelper.map_ignore_located grammar__from__grammar
