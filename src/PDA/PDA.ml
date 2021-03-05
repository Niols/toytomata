open Common
open AST

let push_maybe stack = function
  | None -> stack
  | Some symbol -> Stack.push symbol stack

let one_step (pda : pda) ((state, word, stack) : configuration) =
  List.concat_map
    (fun ((state', letter', symbol'), (new_state', push_symbol')) ->
       try
         assert (state = state');
         let new_word =
           match letter', word with
           | None, word -> word
           | Some letter', letter :: word when letter = letter' -> word
           | _ -> assert false
         in
         match symbol', Stack.pop_opt stack with
         | None, _ ->
           [(new_state', new_word, push_maybe stack push_symbol')]

         | Some symbol', Some (stack_top, stack) when symbol' = stack_top ->
           [(new_state', new_word, push_maybe stack push_symbol')]

         | _ ->
           []
       with
         Assert_failure _ -> [])
    pda.transitions

let rec all_steps pda ((state, word, stack) as conf) =
  if word = [] && List.mem state pda.finals && Stack.is_empty stack then
    [conf]
  else
    one_step pda conf
    |> List.concat_map (all_steps pda)

let accepts pda word =
  all_steps pda (pda.initial, word, Stack.empty) <> []

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
