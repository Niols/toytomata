open Ext

type t = Letter.t list

let all_words (alphabet: Alphabet.t) : t Seq.t =
  let next_words (words: 'a list Seq.t) : 'a list Seq.t =
    Seq.flat_map
      (fun word ->
         Seq.map
           (fun letter -> letter :: word)
           (Alphabet.letters alphabet))
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

let not_all_words (alphabet: Alphabet.t) ~(length_limit: int) : t Seq.t =
  let next_words (words: 'a list Seq.t) : 'a list Seq.t =
    Seq.flat_map
      (fun word ->
         Seq.map
           (fun letter -> letter :: word)
           (Alphabet.letters alphabet))
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
  else List.compare Letter.compare w1 w2

let pp fmt w =
  Format.pp_print_list
    ~pp_sep:(fun _fmt () -> ())
    Format.pp_print_string
    fmt w

let list_to_alphabet wl =
  Alphabet.from_letters_list (List.flatten wl)

let seq_to_alphabet ws =
  list_to_alphabet (List.of_seq ws)
