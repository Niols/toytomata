{
  open Parser
}

let alphanum = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*

let lambda = "lambda" | "λ" | "epsilon" | "ε" | "ϵ"

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = parse
  | white         { read lexbuf }
  | newline       { Lexing.new_line lexbuf; read lexbuf }
  | alphanum      { ALPHANUM (Lexing.lexeme lexbuf) }
  | lambda        { EMPTYWORD }

  | "initial" | "initials" { INITIAL }
  | "final" | "finals"     { FINAL }

  | "via" { VIA }
  | "or"  { OR }

  | "--" { DDASH }
  | "->" { RIGHTARROW }
  | '|'  { PIPE }
  | ';'  { SEMICOLON }
  | ','  { COMMA }

  | eof  { EOF }

  | _    { Common.CSTHelpers.syntax_error "Unexpected character: %s" (Lexing.lexeme lexbuf) }

(* FIXME: brackets for longer states*)
(* FIXME: pos in syntax_error *)
