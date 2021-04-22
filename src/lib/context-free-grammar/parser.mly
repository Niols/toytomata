%{ open Common %}

%token <string> TERMINAL
%token <string> NONTERMINAL
%token EMPTYWORD

%token ENTRYPOINTS

%token RIGHTARROW
%token SEMICOLON
%token NEWLINE
%token COMMA
%token PIPE

%token EOF

%start <CST.cfg'> entrypoint
%%

entrypoint: g=located(cfg) EOF { g }

cfg:
| rules=list(terminated_rule) { rules }
;;

terminated_rule:
| rule=located(rule) SEMICOLON { rule }
| rule=located(rule) NEWLINE   { rule }
;;

rule:
| ENTRYPOINTS vs=separated_nonempty_list(COMMA, located(NONTERMINAL))
  { CST.EntryPoints vs }

| v=located(NONTERMINAL) RIGHTARROW
      cases=separated_nonempty_list(PIPE, located(production))
  { CST.Production (v, cases) }
;;

component:
| t=located(TERMINAL)    { CST.T t }
| v=located(NONTERMINAL) { CST.N v }

production:
| EMPTYWORD     { [] }
| case=nonempty_list(located(component)) { case }
;;

%inline located(X): x=X { CSTHelpers.with_positions $startpos $endpos x }
