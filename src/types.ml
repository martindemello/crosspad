module SMap = Map.Make(String)

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

(* Metadata fields supported by ipuz *)

type metadata_key = 
[ `Copyright
| `Publisher
| `Publication
| `Url
| `Uniqueid
| `Title
| `Intro
| `Explanation
| `Annotation
| `Author
| `Editor
| `Date
| `Notes
| `Difficulty
| `Origin
| `Block
| `Empty
| `Styles
]

type xword = {
  rows : int;
  cols : int;
  grid : square array array;
  clues : clues;
  mutable metadata : (metadata_key * string) list;
}

(* I/O formats for conversion *)

type file = {
  name : string;
  format : string;
}

type converter_input = {
  data : string;
  input_format : string;
  output_format : string;
}

type converter_output = {
  output : string;
  error: string;
}

exception PuzzleFormatError of string

(* Plugin interfaces *)

module type READER = sig
  val read : string -> xword
end

module type WRITER = sig
  val write : xword -> string
end
