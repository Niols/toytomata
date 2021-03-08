open Common
open AST

type word = letter list
type stack = symbol Stack.t

type configuration = state * word * stack

let push_maybe stack = function
  | None -> stack
  | Some symbol -> Stack.push symbol stack

let one_step (pda : pda) ((state, word, stack) : configuration) =
  List.concat_map
    (fun (new_state', (letter', symbol', push_symbol')) ->
       try
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
    (transitions_from state pda)

let rec all_steps pda ((state, word, stack) as conf) =
  if word = [] && is_final state pda && Stack.is_empty stack then
    [conf]
  else
    one_step pda conf
    |> List.concat_map (all_steps pda)
