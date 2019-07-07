(lang dune 1.10)
(generate_opam_files true)
(name crosspad)
(version "0.2")
(source (github martindemello/crosspad))
(license MIT)
(authors "Martin DeMello <martindemello@gmail.com>")
(maintainers "Martin DeMello <martindemello@gmail.com>")

(package
  (name "crosspad")
  (synopsis "Crossword grid manipulation library, file format converter and viewer")
  (description "This library defines an `xword` datatype, a library of useful
functions, and conversions to and from various file formats.

Also included are a command line file conversion tool and a gtk-based viewer.")
  (depends
    (angstrom :dev)
    (bitstring :dev)
    (cmdliner :build)
    (containers :dev)
    (ezjsonm :dev)
    (lablgtk :build)
    (yojson :dev)))
