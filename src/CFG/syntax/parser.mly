%token <string> TERMINAL
%token <string> NONTERMINAL
%token EMPTYWORD

%token START

%token RIGHTARROW
%token SEMICOLON
%token PIPE
%token COMMA

%token EOF

%start <CST.grammar> grammar
%%

grammar:
| rules=list(terminated_rule) EOF { rules }
;;

terminated_rule:
| r=rule SEMICOLON { r }
;;

rule:
| START vs=separated_nonempty_list(COMMA, NONTERMINAL)                 { CST.Start vs }
| v=NONTERMINAL RIGHTARROW
      cases=separated_nonempty_list(PIPE, production_case) { CST.Production (v, cases) }
;;

terminal_or_nonterminal:
| t=TERMINAL    { CST.Terminal t }
| v=NONTERMINAL { CST.NonTerminal v }

production_case:
| EMPTYWORD     { [] }
| case=nonempty_list(terminal_or_nonterminal) { case }
;;
