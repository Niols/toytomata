include List

(** {2 Compatibility Layer for OCaml ≥ 4.12.0} *)

type 'a c_list = 'a list = [] | (::) of 'a * 'a c_list
[@@deriving eq, ord]

let equal = equal_c_list
let compare = compare_c_list

let concat_map f l = flatten (map f l)
