(** {1 Toytomata}

    Various automata-related tools. *)

(** {2 ContextFreeGrammar} *)

type cfg = CFG.cfg

(** {3 Parsing & Printing} *)

let cfg_from_channel_exn = CFG.from_channel_exn
let cfg_from_string_exn = CFG.from_string_exn
let cfg_from_file_exn = CFG.from_file_exn

let pp_cfg = CFG.pp
let cfg_to_channel = CFG.to_channel
let cfg_to_string = CFG.to_string
let cfg_to_file = CFG.to_file

(** {2 PushdownAutomaton} *)

type pda = PDA.pda

(** {3 Parsing & Printing} *)

let pda_from_channel_exn = PDA.from_channel_exn
let pda_from_string_exn = PDA.from_string_exn
let pda_from_file_exn = PDA.from_file_exn

let pp_pda = PDA.pp
let pda_to_channel = PDA.to_channel
let pda_to_string = PDA.to_string
let pda_to_file = PDA.to_file

(** {2 Conversions} *)

let cfg_to_pda = PDA.from_cfg
let pda_to_cfg = PDA.to_cfg

(** {2 Module Aliases} *)

module Common = Common
module CFG = CFG
module PDA = PDA
module LanguagePrefix = LanguagePrefix
