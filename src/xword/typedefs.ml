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
  bar_right: bool;
  bar_down: bool;
}

type clue = int * string

type clues = {
  mutable across : clue list;
  mutable down : clue list;
}

(* Directions for cursor movement on the grid *)
type direction = [`Left | `Right | `Up | `Down | `Across | `Bksp_Ac | `Bksp_Dn ]

type word_direction = [`Across | `Down]

type grid_direction = [`Left | `Right | `Up | `Down]

(* Symmetry for grid structure edits (black squares and bars) *)
type symmetry = SymmNone | Symm90 | Symm180

(* Grid rotational symmetry in quarter turns *)
type rotation = R0 | R1 | R2 | R3

type edit_mode = [`Edit | `Solve]

(* Metadata fields supported by ipuz *)

type metadata_key = [
| `Annotation
| `Author
| `Block
| `Copyright
| `Date
| `Difficulty
| `Editor
| `Empty
| `Explanation
| `Intro
| `Notes
| `Origin
| `Publication
| `Publisher
| `Styles
| `Title
| `Uniqueid
| `Url
| `Unknown
]

(* grid is mutable, and holds the state of construction/solving.
 * solution is conceptually immutable; in solving mode it holds
 * the correct fill and is initialised at file load time *)
type xword = {
  rows : int;
  cols : int;
  grid : square array array;
  solution : square array array;
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

exception PuzzleFormatError of string
exception InternalError of string

(* Plugin interfaces *)

module type READER = sig
  val read : string -> xword
end

module type WRITER = sig
  val write : xword -> string
end
