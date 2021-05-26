(** {1 Chomsky Normal Form}

    Efficient representation of grammars in Chomsky normal forms.

    A context-free grammar is said to be in Chomsky normal form if all of its
    production rules are of the form: [A -> BC] or [A -> a] or [S -> ε] where
    [A], [B] and [C] are nonterminal symbols, the letter [a] is a terminal
    symbol, [S] is the start symbol and [ε] denotes the empty string. Also,
    neither [B] nor [C] may be the start symbol, and the third production rule
    can only appear if [ε] is in the language produced by the grammar. *)

type t

val from_cfg : AST.cfg -> t

type nonterminal

val start : t -> nonterminal

val recognises_empty : t -> bool
(** Whether there is a production [S -> ε]. *)

val iter_terminal_productions : (nonterminal -> AST.terminal -> unit) -> t -> unit
(** Iterate on productions of the form [A -> a]. *)

val iter_nonterminal_productions : (nonterminal -> nonterminal -> nonterminal -> unit) -> t -> unit
(** Iterate on productions of the form [A -> BC]. *)

val number_of_nonterminals : t -> int

val nonterminal_index : nonterminal -> int

val nonterminal_to_ast : t -> nonterminal -> AST.nonterminal
