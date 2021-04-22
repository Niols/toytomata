{
  open Parser
}

let terminal = ['a'-'z'] ['0'-'9']*
let nonterminal = ['A'-'Z'] ['0'-'9']*

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = parse
  | white         { read lexbuf }
  | newline       { Lexing.new_line lexbuf; NEWLINE }
  | terminal      { TERMINAL (Lexing.lexeme lexbuf) }
  | nonterminal   { NONTERMINAL (Lexing.lexeme lexbuf) }

  | "start" | "entrypoint" | "entrypoints" { ENTRYPOINTS }
  | "lambda" | "λ" | "epsilon" | "ε" | "ϵ" { EMPTYWORD }

  | "->" { RIGHTARROW }
  | '|'  { PIPE }
  | ';'  { SEMICOLON }
  | ','  { COMMA }

  | eof  { EOF }

  | _    { Common.CSTHelpers.syntax_error "Unexpected character: %s" (Lexing.lexeme lexbuf) }

(* FIXME: brackets for longer states *)
(* FIXME: pos in syntax_error *)
