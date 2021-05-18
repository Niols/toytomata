module List = ExtList
module Seq = ExtSeq
module Stack = ExtStack

type 'a stack = 'a Stack.t

let pf = Format.printf
let epf = Format.eprintf
let spf = Format.sprintf
let fpf = Format.fprintf

let soi = string_of_int
let ios = int_of_string
