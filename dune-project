(lang dune 3.16)

(name opam-build)

(generate_opam_files true)

(source
 (github mtelvers/opam-build))

(authors "Mark Elvers")

(maintainers "Mark Elvers")

(license MIT)

(package
 (name opam-build)
 (synopsis "Extract the build commands from an opam file.")
 (description "Extract and optionally execute the build commands from an opam file.")
 (depends ocaml dune opam-client))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
