module AST = AST
module Runner = Runner

(** {2 AST} *)

include AST
type t = AST.pda

(** {2 CST Parsing & Printing} *)

let cst_from_lexbuf = Parser.entrypoint Lexer.read

let cst_from_channel ichan = cst_from_lexbuf (Lexing.from_channel ichan)
let cst_from_string str = cst_from_lexbuf (Lexing.from_string str)

let cst_from_file fname =
  let ichan = open_in fname in
  let pda = cst_from_channel ichan in
  close_in ichan;
  pda

let pp_cst = Printer.pp_pda'

let cst_to_string = Format.asprintf "%a" pp_cst

let cst_to_channel ochan pda =
  let fmt = Format.formatter_of_out_channel ochan in
  Printer.pp_pda' fmt pda

let cst_to_file fname pda =
  let ochan = open_out fname in
  cst_to_channel ochan pda;
  close_out ochan

let cst_to_ast = CST_to_AST.pda'__to__pda
let ast_to_cst = AST_to_CST.pda__to__pda'

(** {2 AST Parsing & Printing} *)

let from_channel ichan = cst_from_channel ichan |> cst_to_ast
let from_string str = cst_from_string str |> cst_to_ast
let from_file fname = cst_from_file fname |> cst_to_ast

let pp fmt g = g |> ast_to_cst |> pp_cst fmt
let to_channel ochan g = g |> ast_to_cst |> cst_to_channel ochan
let to_string g = g |> ast_to_cst |> cst_to_string
let to_file fname g = g |> ast_to_cst |> cst_to_file fname

(** {2 Rest} *)

let accepts = Runner.accepts
