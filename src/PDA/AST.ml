open Common

type state = string      [@@deriving show { with_path = false } ]
type letter = string     [@@deriving show { with_path = false } ]
type symbol = string     [@@deriving show { with_path = false } ]

type pda =
  { initials : state list ;
    finals : state list ;
    transitions : ((state * letter option * symbol option) * (state * symbol option)) list }
[@@deriving show { with_path = false } ]

type word = letter list         [@@deriving show { with_path = false } ]
type stack = symbol Stack.t     [@@deriving show { with_path = false } ]

type configuration = state * word * stack [@@deriving show { with_path = false } ]

let empty_pda =
  { initials = []; finals = []; transitions = [] }

let fresh_state =
  let counter = ref 0 in
  fun () ->
    incr counter;
    "q" ^ string_of_int !counter

let add_initial pda s =
  { pda with initials = s :: pda.initials }

let add_initials pda ss =
  { pda with initials = ss @ pda.initials }

let add_final pda s =
  { pda with finals = s :: pda.finals }

let add_finals pda ss =
  { pda with finals = ss @ pda.finals }

let add_transition pda q q' (a, s, s') =
  { pda with transitions = ((q, a, s), (q', s')) :: pda.transitions }

let add_transitions pda q q' ts =
  let ts = List.map (fun (a, s, s') -> ((q, a, s), (q', s'))) ts in
  { pda with transitions = ts @ pda.transitions }
