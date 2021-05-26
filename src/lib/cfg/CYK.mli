open Common
open AST

(** {1 Cocke-Younger-Kasami Algorithm} *)

type parsetree =
  | EProd
  | TProd of terminal
  | NTProd of (CNF.nonterminal * parsetree) * (CNF.nonterminal * parsetree)

val parse : CNF.t -> Word.t -> parsetree option

val accepts : CNF.t -> Word.t -> bool
