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
 (depends
  (ocaml (>= 5.3.0))
  dune
  (opam-client (>= 2.3.0))
  (cmdliner (< 2.0.0))))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
