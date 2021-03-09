open Common

open AST

let l = CSTHelpers.dummily
let ll = List.map l

let pda__to__pda pda =
  (pda |> initial_states |> List.map State.to_string |> ll |> (fun ss -> CST.Initials ss) |> l)
  :: (pda |> final_states |> List.map State.to_string |> ll |> (fun ss -> CST.Finals ss) |> l)
  :: List.map
    (fun (q, q', (a, s, s')) ->
       let q = l (State.to_string q) in
       let q' = l (State.to_string q') in
       let t = (l a, l s, l s') in
       l (CST.Transition (q, q', [t])))
    (transitions_list pda)

let pda__to__pda' pda = l (pda__to__pda pda)
