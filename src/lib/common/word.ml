open Ext

type t = Letter.t array

let empty = [||]
let is_empty = (=) [||]

let letters = Array.to_seq
let letters_list = Array.to_list

let from_letters = Array.of_seq
let from_letters_list = Array.of_list

let all_words ?(length_limit=max_int) alphabet =
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
  |> Seq.map from_letters_list

let equal = Array.equal Letter.equal

let compare w1 w2 =
  let c = Int.compare (Array.length w1) (Array.length w2) in
  if c <> 0 then c
  else Array.compare Letter.compare w1 w2

let pp fmt word =
  Format.pp_print_seq
    ~pp_sep:(fun _fmt () -> ())
    Letter.pp
    fmt (letters word)

let list_to_alphabet word_list =
  word_list
  |> List.concat_map Array.to_list
  |> Alphabet.from_letters_list

let seq_to_alphabet word_list =
  list_to_alphabet (List.of_seq word_list)

let length = Array.length

let concat = Array.append
let add_letter word letter = concat word [|letter|]

let fold_all_prefixes ?(at_new_length=fun _ -> ()) ?(length_limit=max_int) f x alphabet =
  let alphabet = Alphabet.letters_list alphabet in
  let rec aux length states =
    (* states only have words of the same length, sorted with respect to
       Word.compare *)
    if length < length_limit then
      (
        at_new_length length;
        let states =
          states |> List.concat_map @@ fun (state, word) ->
          alphabet |> List.map @@ fun letter ->
          let word = add_letter word letter in
          (f state word letter, word)
        in
        aux (length+1) states
      )
  in
  aux 0 [x, empty]
