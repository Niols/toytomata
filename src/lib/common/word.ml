open Ext

type letter = string

type alphabet = string list

let pp_alphabet sep fmt a =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> fpf fmt "%s" sep)
    Format.pp_print_string
    fmt a

type word = string list
type t = word

let all_words (alphabet: alphabet) : word Seq.t =
  let alphabet = List.to_seq alphabet in
  let next_words (words: 'a list Seq.t) : 'a list Seq.t =
    Seq.flat_map
      (fun word ->
         Seq.map
           (fun letter -> letter :: word)
           alphabet)
      words
  in
  let rec all_words words =
    Seq.Cons (words, fun () -> all_words (next_words words))
  in
  (fun () -> all_words (fun () -> Cons ([], Seq.empty)))
  |> Seq.flatten
  |> Seq.map List.rev
(** The sequence of all words, by increasing length and alphabetical order
   within the same length. *)

let not_all_words (alphabet: alphabet) ~(length_limit: int) : word Seq.t =
  let alphabet = List.to_seq alphabet in
  let next_words (words: 'a list Seq.t) : 'a list Seq.t =
    Seq.flat_map
      (fun word ->
         Seq.map
           (fun letter -> letter :: word)
           alphabet)
      words
  in
  let rec all_words length words =
    if length >= length_limit then
      Seq.Cons (words, fun () -> Seq.Nil)
    else
      Seq.Cons (words, fun () -> all_words (length + 1) (next_words words))
  in
  (fun () -> all_words 0 (fun () -> Cons ([], Seq.empty)))
  |> Seq.flatten
  |> Seq.map List.rev

let compare w1 w2 =
  let c = Int.compare (List.length w1) (List.length w2) in
  if c <> 0 then c
  else List.compare String.compare w1 w2

let pp fmt w =
  Format.pp_print_list
    ~pp_sep:(fun _fmt () -> ())
    Format.pp_print_string
    fmt w
