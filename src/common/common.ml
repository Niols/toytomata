module Stack = Stack
module CSTHelpers = CSTHelpers

type 'a stack = 'a Stack.t

let pf = Format.printf
let spf = Format.sprintf
let fpf = Format.fprintf

let soi = string_of_int
let ios = int_of_string
