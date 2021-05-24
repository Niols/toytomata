open Common

type kind = Complete | Incomplete

type t =
  { kind : kind ;
    words : Word.t list }

let from_word_list ~kind words =
  { kind ; words = List.sort_uniq Word.compare words }

let kind p = p.kind

let is_complete p = (kind p) = Complete
let is_incomplete p = (kind p) = Incomplete

let words p = List.to_seq p.words

let words_list p = p.words
