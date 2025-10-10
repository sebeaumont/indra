(lang dune 3.20)

(name indra)

(generate_opam_files true)

(source
 (github sebeaumont/indra))

(authors "Simon Beaumont <typetyro@icloud.com>")

(maintainers "Hackman <datalligator@icloud.com>")

(license GPL-3.0-or-later)

(documentation https://url/to/documentation)

(package
 (name indra)
 (synopsis "Learning OCaml one complex function at a time")
 (description "Mobius tranformations broke my abacus!")
 (depends ocaml
          owl
          owl-top
          ocaml-canvas
          ppx_deriving
  (alcotest :with-test))
 (tags
  ("Kleinian Groups" "Maths" indra)))

