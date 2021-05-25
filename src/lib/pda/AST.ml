open Ext
open Common

module State = Element.Make(struct end)
type state = State.t

type letter = Letter.t
type symbol = string

module LetterOptionMap = Map.Make(struct
    type t = letter option
    let compare = compare
  end)

type stack_transition = symbol option * symbol option

type pda =
  { initials : state list ;
    finals : state list ;
    transitions : (state * stack_transition) list LetterOptionMap.t State.Map.t }

(** {2 Reading PDAs} *)

let initial_states_list pda = pda.initials
let initial_states pda = List.to_seq (initial_states_list pda)

let is_initial q pda =
  initial_states_list pda |> List.mem q

let final_states_list pda = pda.finals
let final_states pda = List.to_seq (final_states_list pda)

let is_final q pda =
  final_states_list pda |> List.mem q

type transition = letter option * symbol option * symbol option

let all_transitions pda =
  pda.transitions
  |> State.Map.to_seq
  |> Seq.flat_map @@ fun (q, lom) ->
  LetterOptionMap.to_seq lom
  |> Seq.flat_map @@ fun (a, ts) ->
  ts
  |> List.to_seq
  |> Seq.map @@ fun (q', (s, s')) ->
  (q, q', (a, s, s'))

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
  pda.transitions
  |> State.Map.find q
  |> LetterOptionMap.to_seq
  |> Seq.flat_map @@ fun (a, ts) ->
  ts
  |> List.to_seq
  |> Seq.map @@ fun (q', (s, s')) ->
  (q', (a, s, s'))

let transitions_list_from q pda =
  transitions_from q pda |> List.of_seq

let letter_transitions_from q a pda =
  pda.transitions
  |> State.Map.find q
  |> LetterOptionMap.find_opt (Some a)
  |> (function
      | None -> []
      | Some ts -> ts)

let epsilon_transitions_from q pda =
  pda.transitions
  |> State.Map.find q
  |> LetterOptionMap.find_opt None
  |> (function
      | None -> []
      | Some ts -> ts)

let states_list pda =
  Seq.append
    (initial_states pda)
    (Seq.append
       (final_states pda)
       (Seq.flat_map (fun (q, q', _) -> Seq.(cons q (cons q' empty))) (transitions pda)))
  |> List.of_seq
  |> List.sort_uniq State.compare

let states pda = List.to_seq (states_list pda)

let letters pda =
  transitions pda
  |> Seq.filter_map (fun (_, _, (a, _, _)) -> a)
(* notice that a is a letter option, hence the [filter_map] *)

let letters_list pda =
  List.of_seq (letters pda)

let alphabet pda =
  Alphabet.from_letters (letters pda)

let symbols_list pda =
  transitions_list pda
  |> List.concat_map (fun (_, _, (_, s, s')) -> List.filter_map Fun.id [s; s'])
  (* notice that s and s' are symbol options *)
  |> List.sort_uniq compare

let symbols pda =
  List.to_seq (symbols_list pda)

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

let add_transition q q' (a, s, s') pda =
  { pda with
    transitions =
      pda.transitions
      |> State.Map.update q
        (function
          | None -> Some (LetterOptionMap.singleton a [q', (s, s')])
          | Some lom ->
            Some
              (LetterOptionMap.update a
                 (function
                   | None -> Some [q', (s, s')]
                   | Some ts -> Some ((q', (s, s')) :: ts))
                 lom)) }

let add_transitions q q' ts pda =
  List.fold_left (fun pda t -> add_transition q q' t pda) pda ts
