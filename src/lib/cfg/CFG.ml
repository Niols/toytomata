module AST = AST
module Transform = Transform
module CNF = CNF

let key = "CFG"

(** {2 AST} *)

include AST
type t = AST.cfg

(** {2 CST Parsing & Printing} *)

module ParsingFunctions = Common.ParsingFunctions.Make
    (struct
      include Parser
      type output = CST.cfg CST.located
      let lexer_entrypoint = Lexer.read
      let entrypoint = Parser.Incremental.entrypoint
    end)

let cst_from_string = ParsingFunctions.from_string
let cst_from_channel = ParsingFunctions.from_channel
let cst_from_file = ParsingFunctions.from_file

let cst_from_string_exn = ParsingFunctions.from_string_exn
let cst_from_channel_exn = ParsingFunctions.from_channel_exn
let cst_from_file_exn = ParsingFunctions.from_file_exn

let pp_cst = Printer.pp_cfg'

let cst_to_string = Format.asprintf "%a" pp_cst

let cst_to_channel ochan grammar =
  let fmt = Format.formatter_of_out_channel ochan in
  pp_cst fmt grammar

let cst_to_file fname grammar =
  let ochan = open_out fname in
  cst_to_channel ochan grammar;
  close_out ochan

let cst_to_ast = CST_to_AST.cfg'__to__cfg
let ast_to_cst = AST_to_CST.cfg__to__cfg'

(** {2 AST Parsing & Printing} *)

let from_channel_exn ichan = cst_from_channel_exn ichan |> cst_to_ast
let from_string_exn str = cst_from_string_exn str |> cst_to_ast
let from_file_exn fname = cst_from_file_exn fname |> cst_to_ast

let pp fmt g = g |> ast_to_cst |> pp_cst fmt
let to_channel ochan g = g |> ast_to_cst |> cst_to_channel ochan
let to_string g = g |> ast_to_cst |> cst_to_string
let to_file fname g = g |> ast_to_cst |> cst_to_file fname

(** {2 Words Acceptance} *)

let accepts cfg =
  let cfg = Transform.cnf cfg in
  let cnf = CNF.from_cfg cfg in
  fun word ->
    if CNF.accepts cnf word then
      `True
    else
      `False
(** Test of acceptance of a word by a CFG. Note that this function works in two
   steps: given the CFG, it computes its CNF, stores it efficiently and then
   returns a function which, given a word, checks whether it is accepted. This
   means that for repetitive uses of the same function, it is much more
   efficient to store the result of [accepts cfg] and then call this function
   rather than call [accepts cfg word] again on every word. *)
