(lang dune 3.7)
(using menhir 2.1)

(name gkat)

(generate_opam_files true)

(source
 (github tmoux/gkat))

(authors "Timothy Mou")

(license MIT)

; (documentation https://url/to/documentation)

(package
 (name gkat)
 (synopsis
  "An automata-based decision procedure for equivalence of GKAT programs")
 (depends ocaml dune ounit2 qcheck menhir)
 )
 ; (tags
 ;  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
