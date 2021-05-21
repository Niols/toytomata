open Ext

type t = Letter.t list

let pp sep fmt a =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> fpf fmt "%s" sep)
    Letter.pp
    fmt a

let from_letters_list l =
  List.sort_uniq Letter.compare l
