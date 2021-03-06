%{ open Common %}

%token <string> OBJECT

%token EMPTYWORD

%token INITIAL
%token FINAL

%token RIGHTARROW
%token SEMICOLON
%token COMMA
%token DDASH
%token SLASH
%token OR
%token VIA

%token EOF

%start <CST.pda'> entrypoint
%%

entrypoint: p=located(pda) EOF { p }

pda:
| rules=list(terminated_rule) { rules }
;;

terminated_rule:
| r=located(rule) SEMICOLON { r }
;;

rule:
| INITIAL states=separated_nonempty_list(COMMA, located(OBJECT)) { CST.Initial states }
| FINAL states=separated_nonempty_list(COMMA, located(OBJECT))   { CST.Final states }
| q=located(OBJECT) COMMA a=letter_or_empty COMMA s=symbol_or_empty
  RIGHTARROW qp=located(OBJECT) COMMA sp=symbol_or_empty         { CST.Transition ((q, a, s), (qp, sp)) }
| q=located(OBJECT) RIGHTARROW qp=located(OBJECT) VIA transitions=separated_nonempty_list(OR, transition)
                                                                { CST.TransitionVia (q, qp, transitions) }
| q=located(OBJECT) DDASH transition=transition RIGHTARROW qp=located(OBJECT)
                                                                { CST.TransitionArrow (q, transition, qp) }
;;

letter_or_empty:
| e=located(EMPTYWORD) { CSTHelpers.map_located (fun () -> None) e }
| l=located(OBJECT)    { CSTHelpers.map_located (fun l -> Some l) l }
;;

symbol_or_empty:
| e=located(EMPTYWORD) { CSTHelpers.map_located (fun () -> None) e }
| s=located(OBJECT)    { CSTHelpers.map_located (fun s -> Some s) s }
;;

transition:
| a=letter_or_empty COMMA s=symbol_or_empty SLASH sp=symbol_or_empty
                       { (a, s, sp) }
;;

%inline located(X): x=X { CSTHelpers.with_positions $startpos $endpos x }
