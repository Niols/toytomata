open Common

module State = Element.Make(struct end)
type state = State.t

type letter = string
type symbol = string

type transition = letter option * symbol option * symbol option
type letter_transition = letter * symbol option * symbol option
type epsilon_transition = symbol option * symbol option

type pda =
  { initials : state list ;
    finals : state list ;
    transitions :
      ((state * letter_transition) list
       * (state * epsilon_transition) list) State.Map.t }

(** {2 Reading PDAs} *)

let initial_states pda =
  pda.initials

let final_states pda =
  pda.finals

let is_final q pda =
  final_states pda |> List.mem q

let all_transitions pda =
  pda.transitions
  |> State.Map.to_seq
  |> Seq.flat_map
    (fun (q, (lts, ets)) ->
       Seq.append
         (lts |> List.to_seq |> Seq.map (fun (q', (a, s, s')) -> (q, q', (Some a, s, s'))))
         (ets |> List.to_seq |> Seq.map (fun (q', (s, s')) -> (q, q', (None, s, s')))))

let transitions =
  let maybe_filter opt f =
    match opt with
    | None -> Fun.id
    | Some x -> Seq.filter (f x)
  in
  fun ?from_ ?to_ ?that_read ?that_pop ?that_push pda ->
    all_transitions pda
    |> maybe_filter from_     (fun q0  (q,  _, (_, _,  _)) -> q  = q0)
    |> maybe_filter to_       (fun q'0 (_, q', (_, _,  _)) -> q' = q'0)
    |> maybe_filter that_read (fun a0  (_,  _, (a, _,  _)) -> a  = a0)
    |> maybe_filter that_pop  (fun s0  (_,  _, (_, s,  _)) -> s  = s0)
    |> maybe_filter that_push (fun s'0 (_,  _, (_, _, s')) -> s' = s'0)

let transitions_list
  ?from_ ?to_ ?that_read ?that_pop ?that_push pda =
  transitions ?from_ ?to_ ?that_read ?that_pop ?that_push pda
  |> List.of_seq

let transitions_from q pda =
  let (lts, ets) = State.Map.find q pda.transitions in
  List.map (fun (q', (a, s, s')) -> (q', (Some a, s, s'))) lts
  @ List.map (fun (q', (s, s')) -> (q', (None, s, s'))) ets

let letter_transitions_from q pda = fst (State.Map.find q pda.transitions)
let epsilon_transitions_from q pda = snd (State.Map.find q pda.transitions)

let states pda =
  (
    initial_states pda
    @ final_states pda
    @ List.concat_map (fun (q, q', _) -> [q; q']) (transitions_list pda)
  )
  |> List.sort_uniq compare

let letters pda =
  transitions pda
  |> Seq.filter_map (fun (_, _, (a, _, _)) -> a)
  (* notice that a is a letter option *)
  |> List.of_seq
  |> List.sort_uniq compare

let alphabet = letters

let symbols pda =
  transitions_list pda
  |> List.concat_map (fun (_, _, (_, s, s')) -> List.filter_map Fun.id [s; s'])
  (* notice that s and s' are symbol options *)
  |> List.sort_uniq compare

(** {3 Writing PDAs} *)

let empty_pda =
  { initials = []; finals = []; transitions = State.Map.empty }

let fresh_state ?(hint="Q") () =
  State.fresh ~hint

let add_initial s pda =
  { pda with initials = s :: pda.initials }

let add_initials ss pda =
  { pda with initials = ss @ pda.initials }

let add_final s pda =
  { pda with finals = s :: pda.finals }

let add_finals ss pda =
  { pda with finals = ss @ pda.finals }

let add_transition q q' t pda =
  { pda with
    transitions =
      State.Map.update q
        (fun present ->
           let (lts, ets) =
             match present with
             | None -> ([], [])
             | Some (lts, ets) -> (lts, ets)
           in
           match t with
           | (None, s, s') -> Some (lts, (q', (s, s')) :: ets)
           | (Some a, s, s') -> Some ((q', (a, s, s')) :: lts, ets))
        pda.transitions }

let add_transitions q q' ts pda =
  List.fold_left (fun pda t -> add_transition q q' t pda) pda ts
