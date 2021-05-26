module Array = ExtArray
module Seq = ExtSeq
module Stack = ExtStack

type 'a stack = 'a Stack.t

let pf = Format.printf
let epf = Format.eprintf
let spf = Format.sprintf
let fpf = Format.fprintf

let soi = string_of_int
let ios = int_of_string

let fold_for_loop ~from_ ~to_ value f =
  let rec aux i value =
    if i > to_ then value
    else aux (i+1) (f value i)
  in
  aux from_ value
