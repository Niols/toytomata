type t =
  | CFG of CFG.t
  | PDA of PDA.t
  | Prefix of LanguagePrefix.t

let from_cfg cfg = CFG cfg
let from_pda pda = PDA pda
let from_prefix prefix = Prefix prefix
