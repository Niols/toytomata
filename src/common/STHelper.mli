type position
type 'a located [@@deriving show { with_path = false } ]

val dummy_lexing_position : Lexing.position
val dummy_position : position

val with_position : position -> 'a -> 'a located
val with_positions : Lexing.position -> Lexing.position -> 'a -> 'a located

val pp_lexing_position : Format.formatter -> Lexing.position -> unit
val pp_position : Format.formatter -> position -> unit

val dummily : 'a -> 'a located

val map_add_dummy : ('a -> 'b) -> 'a -> 'b located

val value : 'a located -> 'a

val map_located : ('a -> 'b) -> 'a located -> 'b located
val map_ignore_located : ('a -> 'b) -> 'a located -> 'b

val pp_ignore_located : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a located -> unit
(** When [pp] is a printer for values of type ['a], [pp_ignore_located pp] is
   the printer for values of type ['a located] that ignores locations and simply
   call [pp] on the value. *)

exception SyntaxError of position * string

val syntax_error :
  ?start_pos:Lexing.position -> ?end_pos:Lexing.position ->
  ?pos:position ->
  ('a, Format.formatter, unit, 'b) format4 -> 'a

val catch_syntax_error : ('a -> 'b) -> 'a -> 'b
