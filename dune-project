(lang dune 3.17)

(using menhir 3.0)

(subst disabled) ; workaround for build failure on Nix

(name react_trace)

(generate_opam_files true)

(source
 (github Zeta611/react-trace))

(authors "Jay Lee" "Joongwon Ahn")

(maintainers "Jay Lee" "Joongwon Ahn")

(license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name react_trace)
 (synopsis "React-tRace")
 (description "React-tRace is a React Hooks interpreter based on a formal semantics.")
 (depends
  ocaml
  (ocaml-base-compiler
   (= 5.3.0))
  menhir
  dune
  base
  stdio
  string_dict
  ppx_jane
  js_of_ocaml
  js_of_ocaml-ppx
  (flow_parser
   (>= 0.257.0))
  (printbox
   (>= 0.12))
  (printbox-text
   (>= 0.12))
  fmt
  logs
  yojson
  ppx_yojson_conv
  (alcotest :with-test))
 (tags
  (React Hooks "Render semantics" "Definitional interpreter")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
