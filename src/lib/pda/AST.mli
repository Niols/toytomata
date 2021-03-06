open Common

(** {1 PDAs} *)

module State : Element.S

type state = State.t
type letter = Letter.t
type symbol = string

type pda

(** {2 Other Types} *)

type transition = letter option * symbol option * symbol option
type stack_transition = symbol option * symbol option

(** {2 Reading PDAs} *)

val states : pda -> state Seq.t
val states_list : pda -> state list

val initial_states : pda -> state Seq.t
val initial_states_list : pda -> state list

val final_states : pda -> state Seq.t
val final_states_list : pda -> state list

val is_initial : state -> pda -> bool
val is_final : state -> pda -> bool

val letters : pda -> letter Seq.t
val letters_list : pda -> letter list
val alphabet : pda -> Alphabet.t

val symbols : pda -> symbol Seq.t
val symbols_list : pda -> symbol list

val transitions :
  ?from_:state -> ?to_:state ->
  ?that_read:letter option ->
  ?that_pop:symbol option -> ?that_push:symbol option ->
  pda -> (state * state * transition) Seq.t

val transitions_list :
  ?from_:state -> ?to_:state ->
  ?that_read:letter option ->
  ?that_pop:symbol option -> ?that_push:symbol option ->
  pda -> (state * state * transition) list

val transitions_from : state -> pda -> (state * transition) Seq.t
(** [transitions_from q pda] is similar to [transitions ~from_:q pda] except
   that it answers faster and that [q] does not appear in the output. *)

val transitions_list_from : state -> pda -> (state * transition) list

val letter_transitions_from : state -> letter -> pda -> (state * stack_transition) list
val epsilon_transitions_from : state -> pda -> (state * stack_transition) list

(** {2 Creating PDAs} *)

val empty_pda : pda

val fresh_state : ?hint:string -> unit -> state

val add_initial : state -> pda -> pda
val add_initials : state list -> pda -> pda

val add_final : state -> pda -> pda
val add_finals : state list -> pda -> pda

val add_transition : state -> state -> transition -> pda -> pda
val add_transitions : state -> state -> transition list -> pda -> pda
