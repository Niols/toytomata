open Common
open AST

(** {1 Cocke-Younger-Kasami Algorithm}

    Algorithm to check whether a word is recognised by an arbitrary context free
   grammar. It can be modified to also provide a parse tree, which is here the
   case. It runs in [O(n²⋅r)] with [n] the size of the word and [r] the size of
   the grammar. *)

type parsetree =
  | EProd
  | TProd of terminal
  | NTProd of (CNF.nonterminal * parsetree) * (CNF.nonterminal * parsetree)

val parse : CNF.t -> Word.t -> parsetree option
(** Try to parse the given word with the given grammar and to return a parse
   tree. Returns [None] if the word is not recognised. *)

val accepts : CNF.t -> Word.t -> bool
(** Checks whether a given grammar recognises the given word. *)

(** {2 Incremental Version}

    Incremental version of the CYK algorithm. When it is used to parse a whole
   word, it runs in [O(n²⋅r⋅log(max n r))] with [n] the size of the word and [r]
   the size of the grammar. The logarithm comes from the use of maps instead of
   arrays in the implementation. *)

module State : sig
  type t

  val initial : CNF.t -> t

  val parse_letter : t -> Letter.t -> t
  val parse_word : t -> Word.t -> t

  val parsetree : t -> parsetree option
  val accepting : t -> bool
end
