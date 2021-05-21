type t =
  | CFG of CFG.t
  | PDA of PDA.t
  | Prefix of LanguagePrefix.t

let from_cfg cfg = CFG cfg
let from_pda pda = PDA pda
let from_prefix prefix = Prefix prefix

module type S = sig
  type t

  val from_channel_exn : in_channel -> t
  val from_string_exn : string -> t
  val from_file_exn : string -> t

  val to_channel : out_channel -> t -> unit
  val to_string : t -> string
  val to_file : string -> t -> unit
end

type wrapper = Wrapper : (module S with type t = 'a) * ('a -> t) -> wrapper

let all_wrappers = [
  Wrapper ((module CFG), fun cfg -> CFG cfg) ;
  Wrapper ((module PDA), fun pda -> PDA pda) ;
  Wrapper ((module LanguagePrefix), fun prefix -> Prefix prefix) ;
]

type wrapped = Wrapped : (module S with type t = 'a) * 'a -> wrapped

let wrap = function
  | CFG cfg -> Wrapped ((module CFG : S with type t = CFG.t), cfg)
  | PDA pda -> Wrapped ((module PDA : S with type t = PDA.t), pda)
  | Prefix prefix -> Wrapped ((module LanguagePrefix : S with type t = LanguagePrefix.t), prefix)

let first_wrapper_success_exn f =
  let rec first_wrapper_success_exn = function
    | [] -> assert false (* FIXME *)
    | wrapper :: wrappers ->
      try
        f wrapper
      with
        _ -> first_wrapper_success_exn wrappers
  in
  first_wrapper_success_exn all_wrappers

let from_channel_exn ichan =
  first_wrapper_success_exn @@ fun (Wrapper ((module Obj), wrapper)) ->
  wrapper (Obj.from_channel_exn ichan)

let from_string_exn str =
  first_wrapper_success_exn @@ fun (Wrapper ((module Obj), wrapper)) ->
  wrapper (Obj.from_string_exn str)

let from_file_exn fname =
  first_wrapper_success_exn @@ fun (Wrapper ((module Obj), wrapper)) ->
  wrapper (Obj.from_file_exn fname)

let to_channel ochan (Wrapped ((module Obj), obj)) =
  Obj.to_channel ochan obj

let to_string (Wrapped ((module Obj), obj)) =
  Obj.to_string obj

let to_file fname (Wrapped ((module Obj), obj)) =
  Obj.to_file fname obj

let accepts = function
  | CFG cfg -> CFG.accepts cfg
  | PDA pda -> PDA.accepts pda
  | Prefix _ -> assert false
