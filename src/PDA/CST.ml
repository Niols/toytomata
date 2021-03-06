type 'a located = 'a Common.CSTHelpers.located
[@@deriving show { with_path = false }]

type state = string
and letter = string
and symbol = string

and transition = letter_option' * symbol_option' * symbol_option'

and rule =
  | Initial of state' list
  | Final of state' list
  | Transition of (state' * letter_option' * symbol_option') * (state' * symbol_option')
  | TransitionVia of state' * state' * transition list
  | TransitionArrow of state' * transition * state'

and pda =
  rule' list

and state' = state located
and letter_option' = letter option located
and symbol_option' = symbol option located
and rule' = rule located
and pda' = pda located

[@@deriving show { with_path = false }]
