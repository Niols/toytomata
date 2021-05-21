type t = string

let to_string = Fun.id
let from_string = Fun.id
let from_char c = String.make 1 c

let equal = String.equal
let compare = String.compare

let pp = Format.pp_print_string
