open Common

type kind = Complete | Incomplete

type t =
  { kind : kind ;
    words : Word.t list }

let from_word_list ~kind words =
  { kind ; words = List.sort_uniq Word.compare words }

let kind p = p.kind

let words p = List.to_seq p.words
let words_list p = p.words
