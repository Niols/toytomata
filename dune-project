(lang dune 2.8)
(using menhir 2.1)
(generate_opam_files true)

(package
 (name toytomata)
 (depends
  ppx_deriving))
