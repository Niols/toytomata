type state = string
type letter = string
type stack_letter = string

type t =
  { states : state list ;
    alphabet : letter list ;
    initial : state ;
    finals : state list ;
    stack_alphabet : stack_letter list ;
    transitions : ((state * letter option * stack_letter option) * (state * stack_letter option) list) list }

type word = letter list
type stack = stack_letter list

let stack_pop = function
  | [] -> None
  | e :: q -> Some (e, q)

type configuration = state * word * stack

let push_maybe stack = function
  | None -> stack
  | Some stack_letter -> stack_letter :: stack

let one_step pda (state, word, stack) =
  match word with
  | [] -> assert false
  | letter :: word ->
    List.concat_map
      (fun ((state', letter', stack_letter'), outputs') ->
         try
           assert (state = state');
           let new_word =
             match letter' with
             | None -> letter :: word
             | Some letter' when letter = letter' -> word
             | _ -> assert false
           in
           match stack_letter', stack_pop stack with
           | None, _ ->
             List.map
               (fun (new_state', push_letter') ->
                  (new_state', new_word, push_maybe stack push_letter'))
               outputs'

           | Some stack_letter', Some (stack_top, stack) when stack_letter' = stack_top ->
             List.map
               (fun (new_state', push_letter') ->
                  (new_state', new_word, push_maybe stack push_letter'))
               outputs'

           | _ ->
             []
         with
           Assert_failure _ -> [])
      pda.transitions

let rec all_steps pda (state, word, stack) =
  if word = [] then
    [(state, word, stack)]
  else
    one_step pda (state, word, stack)
    |> List.concat_map (all_steps pda)

let accepts pda word =
  all_steps pda (pda.initial, word, [])
  |> List.exists
    (fun (state, _word, stack) ->
       stack = [] && List.mem state pda.finals)
