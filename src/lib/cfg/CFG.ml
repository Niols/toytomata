open Ext

module AST = AST
module Transform = Transform
module CNF = CNF

(** {2 AST} *)

include AST
type cfg = AST.cfg

(** {2 CST Parsing & Printing} *)

module I = Parser.MenhirInterpreter
module E = MenhirLib.ErrorReports

let env checkpoint =
  match checkpoint with
  | I.HandlingError env -> env
  | _ -> assert false

let state checkpoint : int =
  match I.top (env checkpoint) with
  | Some (I.Element (s, _, _, _)) -> I.number s
  | None -> 0

let show text positions =
  E.extract text positions
  |> E.sanitize
  |> E.compress
  |> E.shorten 20 (* max width 43 *)

let succeed v = v

let fail text buffer _checkpoint =
  (* Indicate where in the input file the error occurred. *)
  let location = MenhirLib.LexerUtil.range (MenhirLib.ErrorReports.last buffer) in
  (* Show the tokens just before and just after the error. *)
  let indication = spf "Syntax error %s.\n" (MenhirLib.ErrorReports.show (show text) buffer) in
  (* Show these three components. *)
  epf "%s%s%!" location indication;
  exit 1

let cst_from_string ?(filename="-") text =
  let lexbuf = MenhirLib.LexerUtil.init filename (Lexing.from_string text) in
  let supplier = Parser.MenhirInterpreter.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  let (buffer, supplier) = MenhirLib.ErrorReports.wrap_supplier supplier in
  let checkpoint = Parser.Incremental.entrypoint lexbuf.lex_curr_p in
  Parser.MenhirInterpreter.loop_handle succeed (fail text buffer) supplier checkpoint

let cst_from_channel ?filename ichan =
  let text =
    let buflen = 1024 in
    let out = Buffer.create buflen in
    let buf = Bytes.create buflen in
    let rec read () =
      match input ichan buf 0 buflen with
      | 0 -> ()
      | n -> Buffer.add_subbytes out buf 0 n; read ()
    in
    read ();
    Buffer.contents out
  in
  cst_from_string ?filename text

let cst_from_file fname =
  let ichan = open_in fname in
  let grammar = cst_from_channel ~filename:fname ichan in
  close_in ichan;
  grammar

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

let from_channel ichan = cst_from_channel ichan |> cst_to_ast
let from_string str = cst_from_string str |> cst_to_ast
let from_file fname = cst_from_file fname |> cst_to_ast

let pp fmt g = g |> ast_to_cst |> pp_cst fmt
let to_channel ochan g = g |> ast_to_cst |> cst_to_channel ochan
let to_string g = g |> ast_to_cst |> cst_to_string
let to_file fname g = g |> ast_to_cst |> cst_to_file fname
