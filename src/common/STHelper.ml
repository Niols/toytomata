type lexing_position = Lexing.position = {
  pos_fname : string ;
  pos_lnum  : int ;
  pos_bol   : int ;
  pos_cnum  : int ;
}
[@@deriving show { with_path = false } ]

type position = {
  start_p : lexing_position;
  end_p   : lexing_position
}
[@@deriving show { with_path = false } ]

type 'a located = {
  value    : 'a;
  position : position;
}
[@@deriving show { with_path = false } ]

let dummy_lexing_position =
  { pos_fname = "" ;
    pos_lnum  = -1 ;
    pos_bol   = -1 ;
    pos_cnum  = -1 }

let dummy_position =
  { start_p = dummy_lexing_position ;
    end_p = dummy_lexing_position }

let with_position position value = { value; position }
let with_positions start_p end_p value = with_position { start_p; end_p } value

let pp_gen_position fmt filename linenumber characters =
  let open Format in
  fprintf fmt "%sine %d%s"
    (if filename = "" then "L" else sprintf "File \"%s\", l" filename)
    linenumber
    (match characters with
     | [] -> ""
     | [c] -> sprintf ", character %d" c
     | c1 :: c2 :: _ -> sprintf ", characters %d-%d" c1 c2)

let pp_lexing_position fmt pos =
  pp_gen_position fmt
    pos.pos_fname pos.pos_lnum [pos.pos_cnum - pos.pos_bol]

let pp_position fmt pos =
  pp_gen_position fmt
    pos.start_p.pos_fname
    pos.start_p.pos_lnum
    [pos.start_p.pos_cnum - pos.start_p.pos_bol;
     pos.end_p.pos_cnum - pos.start_p.pos_bol]

let pp_ignore_located pp fmt x = pp fmt x.value

let dummily value = { value ; position = dummy_position }

let map_add_dummy f x = dummily (f x)

let value x = x.value

let map_located f x = { x with value = f x.value }
let map_ignore_located f x = f x.value
