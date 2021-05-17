open Common
open AST

let make__nonterminal'__to__nonterminal () =
  CSTHelpers.map_ignore_located @@
  Converter.make_convert (fun v -> fresh_nonterminal ~hint:v ())

let component__to__component n'2n  = function
  | CST.T a -> T (CSTHelpers.value a)
  | CST.N v -> N (n'2n v)

let component'__to__component n'2n c =
  CSTHelpers.map_ignore_located (component__to__component n'2n) c

let production__to__production n'2n p =
  List.map (component'__to__component n'2n) p

let production'__to__production n'2n p =
  CSTHelpers.map_ignore_located (production__to__production n'2n) p

let rule__to__grammar n'2n cfg = function
  | CST.EntryPoints vs ->
    add_entrypoints
      (List.map n'2n vs)
      cfg

  | Production (v, ps) ->
    add_productions
      (n'2n v)
      (List.map (production'__to__production n'2n) ps)
      cfg

let rule'__to__grammar n'2n cfg rule =
  CSTHelpers.map_ignore_located (rule__to__grammar n'2n cfg) rule

let cfg__to__cfg rules =
  let n'2n = make__nonterminal'__to__nonterminal () in
  List.fold_left (rule'__to__grammar n'2n) empty_cfg rules

let cfg'__to__cfg = CSTHelpers.map_ignore_located cfg__to__cfg
