include module type of Seq

val flatten : 'a t t -> 'a t

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** Same as [List.iter2] but for sequences. The [Invalid_argument] exception is
    raised only when the end is reached. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Same as [List.map2] but for sequences. The [Invalid_argument] exception is
    raised only when the end is reached. *)

val fold_left_pairs : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b t -> 'c t -> 'a
(** Folds over all the pairs made of two elements of the two sequences. *)
