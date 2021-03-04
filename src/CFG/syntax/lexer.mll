{
  open Parser

  exception SyntaxError of string
}

let terminal = ['a'-'z'] ['0'-'9']*
let nonterminal = ['A'-'Z'] ['0'-'9']*

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = parse
  | white         { read lexbuf }
  | newline       { Lexing.new_line lexbuf; read lexbuf }
  | terminal      { TERMINAL (Lexing.lexeme lexbuf) }
  | nonterminal   { NONTERMINAL (Lexing.lexeme lexbuf) }

  | "start" | "entrypoint" { START }
  | "lambda" | "λ" | "epsilon" | "ε" | "ϵ" { EMPTYWORD }

  | "->" { RIGHTARROW }
  | '|'  { PIPE }
  | ';'  { SEMICOLON }
  | ','  { COMMA }

  | eof  { EOF }

  | _    { raise (SyntaxError ("Unexpected character: " ^ (Lexing.lexeme lexbuf))) }

(* FIXME: brackets for longer states*)
