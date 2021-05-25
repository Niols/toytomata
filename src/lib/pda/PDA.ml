module AST = AST
module Runner = Runner

let key = "PDA"

(** {2 AST} *)

include AST
type t = AST.pda

(** {2 CST Parsing & Printing} *)

module ParsingFunctions = Common.ParsingFunctions.Make
    (struct
      include Parser
      type output = CST.pda CST.located
      let lexer_entrypoint = Lexer.read
      let entrypoint = Parser.Incremental.entrypoint
    end)

let cst_from_string = ParsingFunctions.from_string
let cst_from_channel = ParsingFunctions.from_channel
let cst_from_file = ParsingFunctions.from_file

let cst_from_string_exn = ParsingFunctions.from_string_exn
let cst_from_channel_exn = ParsingFunctions.from_channel_exn
let cst_from_file_exn = ParsingFunctions.from_file_exn

let pp_cst = Printer.pp_pda'

let cst_to_string = Format.asprintf "%a" pp_cst

let cst_to_channel ochan pda =
  let fmt = Format.formatter_of_out_channel ochan in
  pp_cst fmt pda

let cst_to_file fname pda =
  let ochan = open_out fname in
  cst_to_channel ochan pda;
  close_out ochan

let cst_to_ast = CST_to_AST.pda'__to__pda
let ast_to_cst = AST_to_CST.pda__to__pda'

(** {2 AST Parsing & Printing} *)

let from_channel_exn ichan = cst_from_channel_exn ichan |> cst_to_ast
let from_string_exn str = cst_from_string_exn str |> cst_to_ast
let from_file_exn fname = cst_from_file_exn fname |> cst_to_ast

let pp fmt g = g |> ast_to_cst |> pp_cst fmt
let to_channel ochan g = g |> ast_to_cst |> cst_to_channel ochan
let to_string g = g |> ast_to_cst |> cst_to_string
let to_file fname g = g |> ast_to_cst |> cst_to_file fname

(** {2 Rest} *)

let to_cfg = PDA_to_CFG.pda_to_cfg
let from_cfg = CFG_to_PDA.cfg_to_pda

let accepts pda = CFG.accepts (to_cfg pda)
