open Ext
open Common

module NonTerminal = Element.Make(struct end)
type nonterminal = NonTerminal.t

type terminal = Letter.t

type component = T of terminal | N of nonterminal

type production = component list

and cfg =
  { entrypoints : nonterminal list ;
    productions : (nonterminal * production) list }

(** {2 Reading} *)

let entrypoints cfg = cfg.entrypoints

let productions ?from_ cfg =
  cfg.productions
  |> List.to_seq
  |> (match from_ with
      | None -> Fun.id
      | Some n -> Seq.filter (fun (n', _) -> NonTerminal.equal n n'))

let productions_list ?from_ cfg =
  List.of_seq (productions ?from_ cfg)

let nonterminals_from_production =
  List.filter_map (function N v -> Some v | _ -> None)

let nonterminals_list cfg =
  cfg.entrypoints
  @ List.concat_map (fun (v, p) -> v :: nonterminals_from_production p) (productions_list cfg)

let nonterminals cfg = List.to_seq (nonterminals_list cfg)

let terminals_from_production =
  List.filter_map (function T a -> Some a | _ -> None)

let terminals_list cfg =
  List.concat_map (fun (_, p) -> terminals_from_production p) (productions_list cfg)
  |> List.sort_uniq compare

let terminals cfg = List.to_seq (terminals_list cfg)

let alphabet cfg =
  Alphabet.from_letters (terminals cfg)

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
