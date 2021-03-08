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

let maybe_filter opt f =
  match opt with
  | None -> Fun.id
  | Some x -> List.filter (f x)

let transitions ?from_ ?to_ ?that_read ?that_pop ?that_push pda =
  pda.transitions
  |> maybe_filter from_     (fun q0  ((q, _, _), ( _,  _)) -> q  = q0)
  |> maybe_filter to_       (fun q'0 ((_, _, _), (q',  _)) -> q' = q'0)
  |> maybe_filter that_read (fun a0  ((_, a, _), ( _,  _)) -> a  = a0)
  |> maybe_filter that_pop  (fun s0  ((_, _, s), ( _,  _)) -> s  = s0)
  |> maybe_filter that_push (fun s'0 ((_, _, _), ( _, s')) -> s' = s'0)

let states pda =
  pda.initials @ pda.finals
  @ List.concat_map (fun ((q, _, _), (q', _)) -> [q; q']) pda.transitions
  |> List.sort_uniq compare
