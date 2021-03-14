open AST

(** {1 Various Transformations of CFG} *)

val extract_entrypoint : cfg -> cfg
(** Eliminate the entrypoint from right-hand sides. Returns a CFG recognising
   the same language but with only one entrypoint that does not appear on the
   right-hand side of rules.*)

val eliminate_terminals : cfg -> cfg
(** Eliminate terminals from right-hand sides, except if there is only one
   terminal and non nonterminal. Returns a CFG recognising the same language but
   with rules producing either only nonterminals or only one terminal. *)

val eliminate_terminals_after_nonterminal : cfg -> cfg
(** Same as {!eliminate_terminals} except only at the right of nonterminals. *)

val limit_to_two_nonterminals : cfg -> cfg
(** Eliminate right-hand sides with more than two nonterminals. Returns a CFG
    recognising the same language but with rules producing at most two
    nonterminals. *)

val inline_epsilon_productions : cfg -> cfg
(** Eliminate ε-productions. Returns a CFG recognising the same language but
   with no ε-production, except possibly on the entrypoints. *)

val merge_unit_productions : cfg -> cfg
(** Eliminate unit productions, that is productions that contain only one
    nonterminal. Returns a CFG recognising the same language but without such
    productions. *)

val remove_unreachable : cfg -> cfg
(** Remove unreachable nonterminals for the grammar. *)

(** {2 Chomsky Normal Form} *)

val start : cfg -> cfg
(** Alias for {extract_entrypoint}. *)

val term : cfg -> cfg
(** Alias for {!eliminate_terminals}. *)

val bin : cfg -> cfg
(** Alias for {!limit_to_two_nonterminals}. *)

val del : cfg -> cfg
(** Alias for {!inline_epsilon_productions}. *)

val unit : cfg -> cfg
(** Alias for {!merge_unit_productions}. *)

val chomsky_normal_form : cfg -> cfg
(** Successive application of start, term, bin, del and unit. Returns a CFG
   recognising the same language but in Chomsky Normal Form, that is with only
   one entrypoint and with only rules of the form [S -> ε], [A -> BC] or [A ->
   a] where [S] is the entrypoint, [A], [B], [C] are nonterminals ([B] and [C]
   cannot be the entrypoint) and [a] is a terminal. *)

val cnf : cfg -> cfg
(** Alias for {!chomsky_normal_form}. *)
