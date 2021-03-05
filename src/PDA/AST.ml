open Common

type state = string      [@@deriving show { with_path = false } ]
type letter = string     [@@deriving show { with_path = false } ]
type symbol = string     [@@deriving show { with_path = false } ]

type pda =
  { initial : state ;
    finals : state list ;
    transitions : ((state * letter option * symbol option) * (state * symbol option)) list }
[@@deriving show { with_path = false } ]

type word = letter list         [@@deriving show { with_path = false } ]
type stack = symbol Stack.t     [@@deriving show { with_path = false } ]

type configuration = state * word * stack [@@deriving show { with_path = false } ]
