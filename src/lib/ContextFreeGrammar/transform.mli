open AST

(** {1 Canonical Transformations of CFG} *)

val start : cfg -> cfg
(** Eliminate the entrypoint from right-hand sides. Returns a CFG recognising
   the same language but with only one entrypoint that does not appear on the
   right-hand side of rules.*)

val term : cfg -> cfg
(** Eliminate terminals from right-hand sides, except if there is only one
   terminal and non nonterminal. Returns a CFG recognising the same language but
   with rules producing either only nonterminals or only one terminal. *)

val term_right : cfg -> cfg
(** Same as {!term} except only at the right of nonterminals. *)

val bin : cfg -> cfg
(** Eliminate right-hand sides with more than two nonterminals. Returns a CFG
   recognising the same language but with rules producing at most two
   nonterminals. *)

val del : cfg -> cfg
(** Eliminate ε-productions. Returns a CFG recognising the same language but
   with no ε-production. *)

val unit : cfg -> cfg
(** Eliminate unit productions, that is productions that contain only one
   nonterminal. Returns a CFG recognising the same language but without such
   productions. *)

val chomsky_normal_form : cfg -> cfg
(** Successive application of start, term, bin, del and unit. Returns a CFG
   recognising the same language but in Chomsky Normal Form, that is with only
   one entrypoint and with only rules of the form [S -> ε], [A -> BC] or [A ->
   a] where [S] is the entrypoint, [A], [B], [C] are nonterminals ([B] and [C]
   cannot be the entrypoint) and [a] is a terminal. *)

val cnf : cfg -> cfg
(** Alias for {!chomsky_normal_form}. *)
