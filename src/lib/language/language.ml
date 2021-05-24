include AST
include Generic

let from_channel_exn ichan =
  first_wrapper_success_exn @@ fun (Wrapper ((module Obj), wrapper)) ->
  wrapper (Obj.from_channel_exn ichan)

let from_string_exn str =
  first_wrapper_success_exn @@ fun (Wrapper ((module Obj), wrapper)) ->
  wrapper (Obj.from_string_exn str)

let from_file_exn fname =
  first_wrapper_success_exn @@ fun (Wrapper ((module Obj), wrapper)) ->
  wrapper (Obj.from_file_exn fname)

let to_channel ochan obj =
  let Wrapped ((module Obj), obj) = wrap obj in
  Obj.to_channel ochan obj

let to_string obj =
  let Wrapped ((module Obj), obj) = wrap obj in
  Obj.to_string obj

let to_file fname obj =
  let Wrapped ((module Obj), obj) = wrap obj in
  Obj.to_file fname obj

let accepts obj =
  let Wrapped ((module Obj), obj) = wrap obj in
  Obj.accepts obj
