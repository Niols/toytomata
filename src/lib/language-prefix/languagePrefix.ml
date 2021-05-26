open Ext
open Common

include AST
include Parser
include Printer

let length p = List.length (words_list p)

let alphabet p =
  words p
  |> Seq.map Word.letters
  |> Seq.flatten
  |> Alphabet.from_letters

let accepts p word =
  let rec accepts words =
    match words () with
    | Seq.Nil when is_complete p -> `False
    | Nil -> `DontKnow `SizeLimit
    | Cons (word', words) ->
      match Word.compare word word' with
      | n when n < 0 -> `False (* because words are sorted *)
      | 0 -> `True
      | _ -> accepts words
  in
  accepts (words p)

module IncrementalAcceptance = struct
  type state = t * Word.t

  let initial prefix = (prefix, Word.empty)

  let parse_letter (prefix, word) letter =
    (prefix, Word.add_letter word letter)

  let parse_word (prefix, word) word' =
    (prefix, Word.concat word word')

  let accepting (prefix, word) =
    accepts prefix word
end
