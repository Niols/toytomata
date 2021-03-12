open Common

module NonTerminal = Element.Make(struct end)
type nonterminal = NonTerminal.t

type terminal = string

type component = T of terminal | N of nonterminal

type production = component list

and cfg =
  { entrypoints : nonterminal list ;
    productions : (nonterminal * production) list }

(** {2 Reading} *)

let entrypoints cfg = cfg.entrypoints

let productions_list cfg = cfg.productions
let productions cfg = productions_list cfg |> List.to_seq

let nonterminals_from_production =
  List.filter_map (function N v -> Some v | _ -> None)

let nonterminals cfg =
  cfg.entrypoints
  @ List.concat_map (fun (v, p) -> v :: nonterminals_from_production p) (productions_list cfg)

let terminals_from_production =
  List.filter_map (function T a -> Some a | _ -> None)

let terminals cfg =
  List.concat_map (fun (_, p) -> terminals_from_production p) (productions_list cfg)

let alphabet = terminals

(** {2 Creating} *)

let empty_cfg =
  { entrypoints = []; productions = [] }

let fresh_nonterminal ?(hint="X") () =
  NonTerminal.fresh ~hint

let add_entrypoint v cfg =
  { cfg with entrypoints = v :: cfg.entrypoints }

let add_entrypoints vs cfg =
  { cfg with entrypoints = vs @ cfg.entrypoints }

let add_production v p cfg =
  { cfg with productions = (v, p) :: cfg.productions }

let add_productions v ps cfg =
  List.fold_left (fun cfg p -> add_production v p cfg) cfg ps

let add_productionss ps cfg =
  List.fold_left (fun cfg (v, p) -> add_production v p cfg) cfg ps

let update_productions f cfg =
  empty_cfg
  |> add_entrypoints (entrypoints cfg)
  |> (cfg
      |> productions_list
      |> List.concat_map (fun (n, p) -> f n p)
      |> add_productionss)
