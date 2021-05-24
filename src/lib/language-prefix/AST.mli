open Common

type kind = Complete | Incomplete

type t

val from_word_list : kind:kind -> Word.t list -> t

val kind : t -> kind
(** Kind of the prefix, complete or incomplete. This specifies whether there are
    more words than the known ones in the language. *)

val is_complete : t -> bool
val is_incomplete : t -> bool

val words : t -> Word.t Seq.t
(** Sequence of words in the prefix. The sequence is finite. The words are given
    in sorted order with respect to {!Word.compare}. *)

val words_list : t -> Word.t list
(** Same as {!words} but the result is given as a list. *)
