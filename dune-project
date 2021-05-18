(lang dune 2.8)

(source (github Niols/toytomata))
(license GPL-3.0-or-later)

(authors "Nicolas “Niols” Jeannerod <niols@niols.fr>")
(maintainers "Nicolas “Niols” Jeannerod <niols@niols.fr>")

(generate_opam_files true)
(using menhir 2.1)

(package
 (name toytomata)
 (synopsis "An executable and library to help with automata courses")
 (depends
  ppx_deriving
  menhir))
