include module type of List

(** {2 Compatibility Layer for OCaml ≥ 4.12.0} *)

val equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
val compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int

val concat_map : ('a -> 'b list) -> 'a list -> 'b list
