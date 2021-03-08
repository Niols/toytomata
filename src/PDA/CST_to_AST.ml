open Common
open AST

let v = CSTHelpers.value

let make__state'__to__state () =
  let table = Hashtbl.create 8 in
  CSTHelpers.map_ignore_located @@ fun q ->
  match Hashtbl.find_opt table q with
  | Some q' -> q'
  | None ->
    let q' = fresh_state () in
    Hashtbl.add table q q';
    q'

let letter__to__letter = Fun.id
let letter'__to__letter a = CSTHelpers.map_ignore_located letter__to__letter a

let symbol__to__symbol = Fun.id
let symbol'__to__symbol a = CSTHelpers.map_ignore_located symbol__to__symbol a

let v3 (a, s, s') = (v a, v s, v s')

let rule__to__pda state'__to__state pda = function
  | CST.Initials qs ->
    add_initials (List.map state'__to__state qs) pda

  | Finals qs ->
    add_finals (List.map state'__to__state qs) pda

  | Transition (q, q', ts) ->
    List.fold_left
      (fun pda (a, s, s') ->
         add_transition
           (state'__to__state q)
           (state'__to__state q')
           (letter'__to__letter a,
            symbol'__to__symbol s,
            symbol'__to__symbol s')
           pda)
      pda
      ts

let rule'__to__pda state'__to__state pda =
  CSTHelpers.map_ignore_located (rule__to__pda state'__to__state pda)

let pda__to__pda rules =
  let state'__to__state = make__state'__to__state () in
  List.fold_left (rule'__to__pda state'__to__state) empty_pda rules

let pda'__to__pda = CSTHelpers.map_ignore_located pda__to__pda
