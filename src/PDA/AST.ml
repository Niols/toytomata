type state = string        [@@deriving show { with_path = false } ]
type letter = string       [@@deriving show { with_path = false } ]
type stack_letter = string [@@deriving show { with_path = false } ]

type t =
  { initial : state ;
    finals : state list ;
    transitions : ((state * letter option * stack_letter option) * (state * stack_letter option)) list }
[@@deriving show { with_path = false } ]

type word = letter list        [@@deriving show { with_path = false } ]
type stack = stack_letter list [@@deriving show { with_path = false } ]

type configuration = state * word * stack [@@deriving show { with_path = false } ]
