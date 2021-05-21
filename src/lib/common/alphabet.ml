open Ext

type t = Letter.t list (* invariant: always sorted *)

let pp sep fmt a =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> fpf fmt "%s" sep)
    Letter.pp
    fmt a

let empty = []

let add_letters_list l a =
  List.sort_uniq Letter.compare (l @ a)

let add_letters s a =
  add_letters_list (List.of_seq s) a

let from_letters_list l =
  add_letters_list l empty

let from_letters s =
  add_letters s empty

let letters a = List.to_seq a

let letters_list a = a

let equal = (=)

let union a1 a2 =
  add_letters (letters a1) a2

let length a = letters_list a |> List.length
