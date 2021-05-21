open Common

(** {1 Context Free Grammars} *)

module NonTerminal : Element.S

type nonterminal = NonTerminal.t
type terminal = Letter.t

type cfg

(** {2 Other Types} *)

type component = T of terminal | N of nonterminal
type production = component list

(** {2 Reading} *)

val entrypoints : cfg -> nonterminal list

val productions :
  ?from_:nonterminal ->
  cfg -> (nonterminal * production) Seq.t

val productions_list :
  ?from_:nonterminal ->
  cfg -> (nonterminal * production) list
(** Same as {!productions} but returns a list. *)

val nonterminals : cfg -> nonterminal Seq.t
val nonterminals_list : cfg -> nonterminal list

val terminals : cfg -> terminal Seq.t
val terminals_list : cfg -> terminal list
val alphabet : cfg -> Alphabet.t

(** {2 Creating} *)

val empty_cfg : cfg

val fresh_nonterminal : ?hint:string -> unit -> nonterminal

val add_entrypoint : nonterminal -> cfg -> cfg
val add_entrypoints : nonterminal list -> cfg -> cfg

val add_production : nonterminal -> production -> cfg -> cfg
val add_productions : nonterminal -> production list -> cfg -> cfg
val add_productionss : (nonterminal * production) list -> cfg -> cfg

val update_productions :
  (nonterminal -> production -> (nonterminal * production) list) -> cfg -> cfg
