type t

val from_cfg : CFG.t -> t
val from_pda : PDA.t -> t
val from_prefix : LanguagePrefix.t -> t

include Generic.S with type t := t
