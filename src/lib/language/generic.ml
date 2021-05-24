open Common
open AST

module type S = sig
  type t

  val from_channel_exn : in_channel -> t
  val from_string_exn : string -> t
  val from_file_exn : string -> t

  val to_channel : out_channel -> t -> unit
  val to_string : t -> string
  val to_file : string -> t -> unit

  val accepts : t -> Word.t -> DecisionResponse.t
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
