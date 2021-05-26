include module type of Array

val fold_lefti : ('a -> int -> 'b -> 'a) -> 'a -> 'b array -> 'a

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
