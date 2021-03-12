(** {1 Context Free Grammars} *)

module NonTerminal : Common.Element.S

type nonterminal = NonTerminal.t
type terminal = string

type cfg

(** {2 Other Types} *)

type component = T of terminal | N of nonterminal
type production = component list

(** {2 Reading} *)

val entrypoints : cfg -> nonterminal list

val productions : cfg -> (nonterminal * production) Seq.t
val productions_list : cfg -> (nonterminal * production) list

val nonterminals : cfg -> nonterminal list

val terminals : cfg -> terminal list
val alphabet : cfg -> terminal list
(** Alias for {!terminals}. *)

(** {2 Creating} *)

val empty_cfg : cfg

val fresh_nonterminal : ?hint:string -> unit -> nonterminal

val add_entrypoint : nonterminal -> cfg -> cfg
val add_entrypoints : nonterminal list -> cfg -> cfg

val add_production : nonterminal -> production -> cfg -> cfg
val add_productions : nonterminal -> production list -> cfg -> cfg

val update_productions :
  (nonterminal -> production -> (nonterminal * production) list) -> cfg -> cfg
