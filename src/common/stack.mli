type 'a t

val empty : 'a t

val push : 'a -> 'a t -> 'a t

val pop : 'a t -> 'a

val pop_opt : 'a t -> 'a option
