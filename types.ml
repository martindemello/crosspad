type letter = string

type rebus = {
  symbol: int;
  solution: string;
  display_char: string
}

type cell = Black | Empty | Letter of letter | Rebus of rebus

type square = {
  cell : cell;
  num : int;
}

type clues = {
  mutable across : string list;
  mutable down : string list;
}

(* Directions for cursor movement on the grid *)
type direction = [`Left | `Right | `Up | `Down | `Across | `Bksp_Ac | `Bksp_Dn ]

type word_direction = [`Across | `Down]

type xword = {
  rows : int;
  cols : int;
  grid : square array array;
  clues : clues;
  mutable metadata : (string * string) list;
}

type file = {
  name : string;
  format : string;
}

exception PuzzleFormatError of string

(* Plugin interfaces *)

module type READER = sig
  val read : string -> xword
end

module type WRITER = sig
  val write : xword -> string
end
