open Common
open AST

let l = CSTHelpers.dummily
let ll = List.map l

let component__to__component = function
  | T a -> CST.T (l (Letter.to_string a))
  | N v -> CST.N (l (NonTerminal.to_string v))

let component__to__component' c =
  l (component__to__component c)

let production__to__production (v, p) =
  CST.Production (
    l (NonTerminal.to_string v),
    [l (List.map component__to__component' p)]
  )

let cfg__to__cfg cfg =
  CST.EntryPoints (cfg |> entrypoints |> List.map NonTerminal.to_string |> ll)
  :: List.map production__to__production (productions_list cfg)

let cfg__to__cfg' cfg = l (cfg__to__cfg cfg)
