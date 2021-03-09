open Common

module State = Element.Make(struct end)
type state = State.t

type letter = string
type symbol = string

type transition = letter option * symbol option * symbol option

type pda =
  { initials : state list ;
    finals : state list ;
    transitions : (state * transition) list State.Map.t }

(** {2 Reading PDAs} *)

let initial_states pda =
  pda.initials

let final_states pda =
  pda.finals

let is_final q pda =
  final_states pda |> List.mem q

let transitions =
  let maybe_filter opt f =
    match opt with
    | None -> Fun.id
    | Some x -> Seq.filter (f x)
  in
  fun ?from_ ?to_ ?that_read ?that_pop ?that_push pda ->
    State.Map.to_seq pda.transitions
    |> Seq.flat_map (fun (q, ts) -> ts |> List.to_seq |> Seq.map (fun (q', t) -> (q, q', t)))
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
  State.Map.find q pda.transitions

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
        (function
          | None -> Some [(q', t)]
          | Some ts -> Some ((q', t) :: ts))
        pda.transitions }

let add_transitions q q' ts pda =
  let ts = List.map (fun t -> (q', t)) ts in
  { pda with
    transitions =
      State.Map.update q
        (function
          | None -> Some ts
          | Some ts' -> Some (ts @ ts'))
        pda.transitions }
