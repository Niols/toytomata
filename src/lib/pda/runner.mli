open Common

open AST

(** {2 Incremental Interface} *)

type configuration

val initial : pda -> configuration

val step_letter : Letter.t -> configuration -> configuration

val steps_word : Word.t -> configuration -> configuration
(** Loops {!step_letter} on a whole word. *)

val accepting : configuration -> bool
(** Checks whether the given configuration is accepting. *)

(** {2 Monolithic Interface} *)

val accepts : pda -> Word.t -> bool
(** Checks whether the given automaton accepts the given word. [accepts pda
   word] is simply [pda |> initial |> steps_word word |> accepting]. *)
