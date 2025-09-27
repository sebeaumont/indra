(Lang dune 3.20)

(name indra)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name indra)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml
          owl
          owl-top
          ocaml-canvas
          ppx_deriving)
 (tags
  ("add topics" "to describe" your project)))

