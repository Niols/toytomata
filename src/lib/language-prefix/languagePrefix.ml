open Ext
open Common

include AST
include Parser

let is_complete p = (kind p) = Complete
let is_incomplete p = (kind p) = Incomplete

let length p = List.length (words_list p)

let alphabet p =
  words p
  |> Seq.map Word.letters
  |> Seq.flatten
  |> Alphabet.from_letters
