type 'a t [@@deriving show]

val empty : 'a t

val is_empty : 'a t -> bool

val push : 'a -> 'a t -> 'a t

val pop : 'a t -> ('a * 'a t)

val pop_opt : 'a t -> ('a * 'a t) option
