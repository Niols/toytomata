%{ open Common %}

%token <string> OBJECT

%token EMPTYWORD

%token INITIAL
%token FINAL

%token RIGHTARROW
%token SEMICOLON
%token NEWLINE
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
| rule=located(rule) SEMICOLON { rule }
| rule=located(rule) NEWLINE   { rule }
;;

rule:
| INITIAL states=separated_nonempty_list(COMMA, located(OBJECT)) { CST.Initials states }
| FINAL states=separated_nonempty_list(COMMA, located(OBJECT))   { CST.Finals states }
| q=located(OBJECT) RIGHTARROW qp=located(OBJECT) VIA transitions=separated_nonempty_list(OR, transition)
                                                                { CST.Transition (q, qp, transitions) }
| q=located(OBJECT) DDASH transition=transition RIGHTARROW qp=located(OBJECT)
                                                                { CST.Transition (q, qp, [transition]) }
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
