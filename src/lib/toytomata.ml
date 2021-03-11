(** {1 Toytomata}

    Various automata-related tools. *)

(** {2 ContextFreeGrammar} *)

type cfg = ContextFreeGrammar.cfg

(** {3 Parsing & Printing} *)

let cfg_from_channel = ContextFreeGrammar.from_channel
let cfg_from_string = ContextFreeGrammar.from_string
let cfg_from_file = ContextFreeGrammar.from_file

let pp_cfg = ContextFreeGrammar.pp
let cfg_to_channel = ContextFreeGrammar.to_channel
let cfg_to_string = ContextFreeGrammar.to_string
let cfg_to_file = ContextFreeGrammar.to_file

(** {2 PushdownAutomaton} *)

type pda = PushdownAutomaton.pda

(** {3 Parsing & Printing} *)

let pda_from_channel = PushdownAutomaton.from_channel
let pda_from_string = PushdownAutomaton.from_string
let pda_from_file = PushdownAutomaton.from_file

let pp_pda = PushdownAutomaton.pp
let pda_to_channel = PushdownAutomaton.to_channel
let pda_to_string = PushdownAutomaton.to_string
let pda_to_file = PushdownAutomaton.to_file

(** {2 Conversions} *)

let cfg_to_pda = Convert.CFG_to_PDA.cfg_to_pda
let pda_to_cfg = Convert.PDA_to_CFG.pda_to_cfg

(** {2 Module Aliases} *)

module Common = Common
module Convert = Convert
module ContextFreeGrammar = ContextFreeGrammar
module PushdownAutomaton = PushdownAutomaton
module CFG = ContextFreeGrammar
module PDA = PushdownAutomaton
