module AST = AST

(** {2 AST} *)

include AST
type t = AST.cfg

(** {2 CST Parsing & Printing} *)

let cst_from_lexbuf = Parser.entrypoint Lexer.read

let cst_from_channel ichan = cst_from_lexbuf (Lexing.from_channel ichan)
let cst_from_string str = cst_from_lexbuf (Lexing.from_string str)

let cst_from_file fname =
  let ichan = open_in fname in
  let grammar = cst_from_channel ichan in
  close_in ichan;
  grammar

let pp_cst = Printer.pp_grammar

let cst_to_string = Format.asprintf "%a" pp_cst

let cst_to_channel ochan grammar =
  let fmt = Format.formatter_of_out_channel ochan in
  Printer.pp_grammar fmt grammar

let cst_to_file fname grammar =
  let ochan = open_out fname in
  cst_to_channel ochan grammar;
  close_out ochan

let cst_to_ast = CST_to_AST.grammar'__to__grammar
let ast_to_cst = AST_to_CST.grammar__to__grammar

(** {2 AST Parsing & Printing} *)

let from_channel ichan = cst_from_channel ichan |> cst_to_ast
let from_string str = cst_from_string str |> cst_to_ast
let from_file fname = cst_from_file fname |> cst_to_ast

let pp fmt g = g |> ast_to_cst |> pp_cst fmt
let to_channel ochan g = g |> ast_to_cst |> cst_to_channel ochan
let to_string g = g |> ast_to_cst |> cst_to_string
let to_file fname g = g |> ast_to_cst |> cst_to_file fname

(** {2 Others} *)

let terminals_from_terminal_or_nonterminal = function
  | Terminal a -> [a]
  | _ -> []

let nonterminals_from_terminal_or_nonterminal = function
  | NonTerminal v -> [v]
  | _ -> []

let terminals_from_rule rule =
  List.concat_map terminals_from_terminal_or_nonterminal rule.rhs

let nonterminals_from_rule rule =
  rule.lhs
  :: List.concat_map
    nonterminals_from_terminal_or_nonterminal
    rule.rhs

let terminals_from_grammar grammar =
  List.concat_map terminals_from_rule grammar.rules
  |> List.sort_uniq compare

let nonterminals_from_grammar grammar =
  grammar.entrypoints
  @ List.concat_map nonterminals_from_rule grammar.rules
  |> List.sort_uniq compare

(** FIXME: grammar can be inconsistent like nonterminals without rules and all *)

let fresh_nonterminal =
  let counter = ref 0 in
  fun () ->
    incr counter;
    "X" ^ string_of_int !counter
