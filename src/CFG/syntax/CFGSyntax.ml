module Lexer = Lexer
module Parser = Parser
module CST = CST
module Printer = Printer

let from_lexbuf = Parser.grammar Lexer.read

let from_channel ichan = from_lexbuf (Lexing.from_channel ichan)
let from_string str = from_lexbuf (Lexing.from_string str)

let from_file fname =
  let ichan = open_in fname in
  let grammar = from_channel ichan in
  close_in ichan;
  grammar

let to_string = Format.asprintf "%a" Printer.pp_grammar

let to_channel ochan grammar =
  let fmt = Format.formatter_of_out_channel ochan in
  Printer.pp_grammar fmt grammar

let to_file fname grammar =
  let ochan = open_out fname in
  to_channel ochan grammar;
  close_out ochan
