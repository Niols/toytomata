
type t

val empty : t
val letters : t -> Letter.t Seq.t
val letters_list : t -> Letter.t list

val from_letters : Letter.t Seq.t -> t
val from_letters_list : Letter.t list -> t

val all_words : Alphabet.t -> t Seq.t
val not_all_words : length_limit:int -> Alphabet.t -> t Seq.t

val equal : t -> t -> bool
val compare : t -> t -> int

val pp : Format.formatter -> t -> unit

val seq_to_alphabet : t Seq.t -> Alphabet.t
val list_to_alphabet : t list -> Alphabet.t
