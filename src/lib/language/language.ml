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

let alphabet obj =
  let Wrapped ((module Obj), obj) = wrap obj in
  Obj.alphabet obj

let accepts obj =
  let Wrapped ((module Obj), obj) = wrap obj in
  Obj.accepts obj

module IncrementalAcceptance = struct
  type state =
      State :
        (module Generic.S with type IncrementalAcceptance.state = 'state)
        * 'state
        -> state

  let initial obj =
    let Wrapped ((module Obj), obj) = wrap obj in
    State ((module Obj), Obj.IncrementalAcceptance.initial obj)

  let parse_letter state letter =
    let State ((module Obj), state) = state in
    State ((module Obj), Obj.IncrementalAcceptance.parse_letter state letter)

  let parse_word state word =
    let State ((module Obj), state) = state in
    State ((module Obj), Obj.IncrementalAcceptance.parse_word state word)

  let accepting state =
    let State ((module Obj), state) = state in
    Obj.IncrementalAcceptance.accepting state
end
