type t

val to_string : t -> string

val from_char : char -> t
val from_string : string -> t

val equal : t -> t -> bool
val compare : t -> t -> int

val pp : Format.formatter -> t -> unit
