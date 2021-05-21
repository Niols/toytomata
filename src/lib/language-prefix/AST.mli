open Common

type kind = Complete | Incomplete

type t

val from_word_list : kind:kind -> Word.t list -> t

val kind : t -> kind

val words : t -> Word.t Seq.t
val words_list : t -> Word.t list
