open Common
open AST

type word = letter list
type stack = symbol Stack.t

module StateStackSet = Set.Make(struct
    type t = (state * stack)
    let compare = compare (* FIXME: avoid polymorphic compare, especially on states *)
  end)

type configuration = pda * StateStackSet.t

let initial pda =
  let conf =
    initial_states pda
    |> List.map (fun q -> (q, Stack.empty))
    |> StateStackSet.of_list
  in
  (pda, conf)

let pop_maybe pi s =
  match s with
  | None -> Some pi
  | Some s ->
    match Stack.pop_opt pi with
    | Some (s', pi) when s = s' -> Some pi
    | _ -> None

let push_maybe stack = function
  | None -> stack
  | Some symbol -> Stack.push symbol stack

let step_only_letter a (pda, confs) =
  confs
  |> StateStackSet.to_seq
  |> Seq.flat_map
    (fun (q, pi) ->
       Seq.filter_map
         (fun (q', (b, s, s')) ->
            if b <> Some a then
              None
            else
              match pop_maybe pi s with
              | None -> None
              | Some pi -> Some (q', push_maybe pi s'))
         (transitions_from q pda
          |> List.to_seq))
  |> StateStackSet.of_seq
  |> (fun confs -> (pda, confs))

let steps_only_empty (pda, confs) =
  let rec one_empty_step confs_done (q, pi) =
    if StateStackSet.mem (q, pi) confs_done then
      confs_done
    else
      let new_confs =
        Seq.filter_map
          (fun (q', (b, s, s')) ->
             if b <> None then
               None
             else
               match pop_maybe pi s with
               | None -> None
               | Some pi -> Some (q', push_maybe pi s'))
          (transitions_from q pda
           |> List.to_seq)
      in
      all_empty_steps
        (StateStackSet.add (q, pi) confs_done)
        new_confs
  and all_empty_steps confs_done confs_to_do =
    Seq.fold_left one_empty_step confs_done confs_to_do
  in
  (pda, all_empty_steps StateStackSet.empty (StateStackSet.to_seq confs))

(* Since we can never be sure that the empty transitions have been done on a
   configuration, we always need to start with them. Therefore, so as to avoid
   useless computation, we will never do them at the end. The acceptance
   function will have to do the empty transitions before checking. *)

let step_letter a conf = conf |> steps_only_empty |> step_only_letter a

let steps_word word conf =
  List.fold_left (fun conf a -> step_letter a conf) conf word

let accepting conf =
  let (pda, confs) = steps_only_empty conf in
  StateStackSet.exists
    (fun (q, pi) -> is_final q pda && Stack.is_empty pi)
    confs

let accepts pda word =
  pda |> initial |> steps_word word |> accepting
