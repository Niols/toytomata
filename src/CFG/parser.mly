%{ open Common %}

%token <string> TERMINAL
%token <string> NONTERMINAL
%token EMPTYWORD

%token ENTRYPOINTS

%token RIGHTARROW
%token SEMICOLON
%token PIPE
%token COMMA

%token EOF

%start <CST.grammar'> entrypoint
%%

entrypoint: g=located(grammar) EOF { g }

grammar:
| rules=list(terminated_rule) { rules }
;;

terminated_rule:
| r=located(rule) SEMICOLON { r }
;;

rule:
| ENTRYPOINTS vs=separated_nonempty_list(COMMA, located(NONTERMINAL))
  { CST.EntryPoints vs }

| v=located(NONTERMINAL) RIGHTARROW
      cases=separated_nonempty_list(PIPE, located(production_case))
  { CST.Production (v, cases) }
;;

terminal_or_nonterminal:
| t=located(TERMINAL)    { CST.Terminal t }
| v=located(NONTERMINAL) { CST.NonTerminal v }

production_case:
| EMPTYWORD     { [] }
| case=nonempty_list(located(terminal_or_nonterminal)) { case }
;;

%inline located(X): x=X { CSTHelpers.with_positions $startpos $endpos x }
