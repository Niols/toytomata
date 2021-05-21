type t

val pp : string -> Format.formatter -> t -> unit

val empty : t

val from_letters : Letter.t Seq.t -> t
val from_letters_list : Letter.t list -> t

val add_letters : Letter.t Seq.t -> t -> t
val add_letters_list : Letter.t list -> t -> t

val letters : t -> Letter.t Seq.t
val letters_list : t -> Letter.t list

val equal : t -> t -> bool
