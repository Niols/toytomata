open Common
open AST

let v = CSTHelpers.value

let state__to__state = Fun.id
let state'__to__state s = CSTHelpers.map_ignore_located state__to__state s

let letter__to__letter = Fun.id
let letter'__to__leter a = CSTHelpers.map_ignore_located letter__to__letter a

let symbol__to__symbol = Fun.id
let symbol'__to__symbol a = CSTHelpers.map_ignore_located symbol__to__symbol a

let v3 (a, s, s') = (v a, v s, v s')

let rule__to__pda pda = function
  | CST.Initial qs -> add_initials pda (List.map v qs)
  | Final qs -> add_finals pda (List.map v qs)
  | Transition ((q, a, s), (q', s')) -> add_transition pda (v q) (v q') (v3 (a, s, s'))
  | TransitionVia (q, q', ts) -> add_transitions pda (v q) (v q') (List.map v3 ts)
  | TransitionArrow (q, t, q') -> add_transition pda (v q) (v q') (v3 t)

let rule'__to__pda pda = CSTHelpers.map_ignore_located (rule__to__pda pda)

let pda__to__pda rules =
  List.fold_left rule'__to__pda empty_pda rules

let pda'__to__pda = CSTHelpers.map_ignore_located pda__to__pda
