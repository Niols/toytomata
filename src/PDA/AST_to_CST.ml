open Common

open AST

let l = CSTHelpers.dummily
let ll = List.map l

let pda__to__pda pda =
  l (CST.Initial (ll pda.initials))
  :: l (CST.Final (ll pda.finals))
  :: List.map
    (fun ((q, a, s), (q', s')) ->
       l (CST.Transition ((l q, l a, l s), (l q', l s'))))
    pda.transitions

let pda__to__pda' pda = l (pda__to__pda pda)
