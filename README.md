Toytomata
=========

*Work in Progress*

Toytomata is an executable and a library that aim at helping with automata
courses. The goal is to allow both students and teachers to enter
language-related objects (automata, regular expressions, grammars, etc.) in a
simple way and to run some usual operations on them. The main usage is to help
check that different objects (and automaton and a grammar, for instance)
recognise the same language, or to give a counter-example. This can be useful
when grading, for instance, to compare a solution with the problem statement.

The API documentation is available [here](https://niols.github.io/toytomata/).

See Also
--------

- [Automata Tutor v3](https://automata-tutor.model.in.tum.de/), a website
  containing automatically-checked exercises on language-related objects.

- [Automaton Simulator](http://automatonsimulator.com/), a website which allows
  you to define automata using a graphical interface and to check whether they
  recognise words or not.

- [CFG](https://mmottl.github.io/cfg/), an OCaml library for analysing and
  manipulating context-free grammars.

References
----------

**[Aho *et al.* 1983]** Alfred V. Aho, John E. Hopcroft and Jeffrey D. Ullman.
*Data Structures and Algorithms*. Addison-Wesley, 1983.

**[Lange & Leiß 2009]** Martin Lange and Hans Leiß. “To CNF or not to CNF? An
Efficient Yet Presentable Version of the CYK Algorithm”. In *Informatica
Didactica*, volume 8, 2009.
http://ddi.cs.uni-potsdam.de/InformaticaDidactica/LangeLeiss2009.pdf

**[Paull & Unger 1968]** Marvin C. Paull and Stephen H. Unger. “Structural
equivalence of context-free grammars”. In *Journal of Computer and System
Sciences*, volume 2, issue 4, December 1968, pages 427-463.
https://doi.org/10.1016/S0022-0000(68)80037-6
