type cfg = CFG.cfg
type pda = PDA.pda

type t =
  | CFG of cfg
  | PDA of pda

let from_cfg cfg = CFG cfg
let from_pda pda = PDA pda

let from_channel ichan =
  try CFG (CFG.from_channel_exn ichan)
  with _ -> PDA (PDA.from_channel_exn ichan)

let from_string str =
  try CFG (CFG.from_string_exn str)
  with _ -> PDA (PDA.from_string_exn str)

let from_file fname =
  try CFG (CFG.from_file_exn fname)
  with _ -> PDA (PDA.from_file_exn fname)

let to_channel ochan = function
  | CFG cfg -> CFG.to_channel ochan cfg
  | PDA pda -> PDA.to_channel ochan pda

let to_string = function
  | CFG cfg -> CFG.to_string cfg
  | PDA pda -> PDA.to_string pda

let to_file fname = function
  | CFG cfg -> CFG.to_file fname cfg
  | PDA pda -> PDA.to_file fname pda

let accepts repr _word =
  match repr with
  | CFG _cfg -> assert false
  | PDA pda ->
    let _cfg = Convert.PDA_to_CFG.pda_to_cfg pda in
    assert false
