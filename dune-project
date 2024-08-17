(lang dune 3.6)

(name chess_engine)

(generate_opam_files true)

(authors "Mathias Anseaume")

(maintainers "Mathias Anseaume")

(license GPL-3.0-only)

(package
 (name chess_engine)
 (synopsis "OCaml chess checkmate solver and endgame tablebase generator")
 (description
  "Project developed for a TIPE on legal move generation, mate-in-N search, and retrograde analysis for chess endgames.")
 (depends
  ocaml
  dune
  unix
  bimage
  bimage-unix)
 (tags
  (chess ocaml bitboards tablebases retrograde-analysis tipe)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
