open ContextFreeGrammar
open PushdownAutomaton

type t =
  | CFG of cfg
  | PDA of pda

let from_cfg cfg = CFG cfg
let from_pda pda = PDA pda

let from_context_free_grammar = from_cfg
let from_pushdown_automaton = from_pda

let from_channel ichan =
  try CFG (ContextFreeGrammar.from_channel ichan)
  with _ -> PDA (PushdownAutomaton.from_channel ichan)

let from_string str =
  try CFG (ContextFreeGrammar.from_string str)
  with _ -> PDA (PushdownAutomaton.from_string str)

let from_file fname =
  try CFG (ContextFreeGrammar.from_file fname)
  with _ -> PDA (PushdownAutomaton.from_file fname)

let pp fmt g = g |> ast_to_cst |> pp_cst fmt

let to_channel ochan = function
  | CFG cfg -> ContextFreeGrammar.to_channel ochan cfg
  | PDA pda -> PushdownAutomaton.to_channel ochan pda

let to_string = function
  | CFG cfg -> ContextFreeGrammar.to_string cfg
  | PDA pda -> PushdownAutomaton.to_string pda

let to_file fname = function
  | CFG cfg -> ContextFreeGrammar.to_file fname cfg
  | PDA pda -> PushdownAutomaton.to_file fname pda
