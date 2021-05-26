(** {1 Abstract Type for Words} *)

type t

val empty : t
val is_empty : t -> bool

val letters : t -> Letter.t Seq.t
val letters_list : t -> Letter.t list

val from_letters : Letter.t Seq.t -> t
val from_letters_list : Letter.t list -> t

val all_words : ?length_limit:int -> Alphabet.t -> t Seq.t
(** The sequence of all words, by increasing length and alphabetical order
    within the same length. *)

val equal : t -> t -> bool
val compare : t -> t -> int

val pp : Format.formatter -> t -> unit

val seq_to_alphabet : t Seq.t -> Alphabet.t
val list_to_alphabet : t list -> Alphabet.t

val length : t -> int

val add_letter : t -> Letter.t -> t
val concat : t -> t -> t

val fold_all_prefixes :
  ?at_new_length:(int -> unit) ->
  ?length_limit:int ->
  ('a -> t -> Letter.t -> 'a) -> 'a -> Alphabet.t -> unit
(** [fold_all_prefixes f x a] calls [f] on all the words except the empty word,
   in an order compatible with {!compare}, passing [f] a state and a letter. The
   state corresponds to the result of the previous run on [f] on the longest
   strict prefix of the word. The letter is the last letter of the word. *)
