type state = string        [@@deriving show { with_path = false } ]
type letter = string       [@@deriving show { with_path = false } ]
type stack_letter = string [@@deriving show { with_path = false } ]

type t =
  { initial : state ;
    finals : state list ;
    transitions : ((state * letter option * stack_letter option) * (state * stack_letter option)) list }
[@@deriving show { with_path = false } ]

type word = letter list        [@@deriving show { with_path = false } ]
type stack = stack_letter list [@@deriving show { with_path = false } ]

let stack_pop = function
  | [] -> None
  | e :: q -> Some (e, q)

type configuration = state * word * stack [@@deriving show { with_path = false } ]

let push_maybe stack = function
  | None -> stack
  | Some stack_letter -> stack_letter :: stack

let one_step pda (state, word, stack) =
  List.concat_map
    (fun ((state', letter', stack_letter'), (new_state', push_letter')) ->
       try
         assert (state = state');
         let new_word =
           match letter', word with
           | None, word -> word
           | Some letter', letter :: word when letter = letter' -> word
           | _ -> assert false
         in
         match stack_letter', stack_pop stack with
         | None, _ ->
           [(new_state', new_word, push_maybe stack push_letter')]

         | Some stack_letter', Some (stack_top, stack) when stack_letter' = stack_top ->
           [(new_state', new_word, push_maybe stack push_letter')]

         | _ ->
           []
       with
         Assert_failure _ -> [])
    pda.transitions

let rec all_steps n pda ((state, word, _) as conf) =
  Format.eprintf "%s%a" (String.make (2*n) ' ') pp_configuration conf;
  if word = [] && List.mem state pda.finals then
    (
      Format.eprintf " <-- returned@.";
      [conf]
    )
  else
    (
      Format.eprintf "@.";
      one_step pda conf
      |> List.concat_map (all_steps (n+1) pda)
    )

let accepts pda word =
  let res = all_steps 0 pda (pda.initial, word, []) in
  Format.eprintf "Result #: %d@." (List.length res);
  res <> []

let fresh_state =
  let counter = ref 0 in
  fun () ->
    incr counter;
    "q" ^ string_of_int !counter

let make_trivial state =
  { initial = state ; finals = [] ; transitions = [] }

let add_final pda state =
  { pda with finals = state :: pda.finals }

let add_transition pda from_ to_ =
  { pda with transitions = (from_, to_) :: pda.transitions }
