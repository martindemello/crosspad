module Types :
  sig
    module SMap = Typedefs.SMap
    type letter = string
    type rebus =
      Typedefs.rebus = {
      symbol : int;
      solution : string;
      display_char : string;
    }
    type cell =
      Typedefs.cell =
        Black
      | Empty
      | Letter of letter
      | Rebus of rebus
    type square = Typedefs.square = { cell : cell; num : int; }
    type clues =
      Typedefs.clues = {
      mutable across : string list;
      mutable down : string list;
    }
    type direction =
        [ `Across | `Bksp_Ac | `Bksp_Dn | `Down | `Left | `Right | `Up ]
    type word_direction = [ `Across | `Down ]
    type metadata_key =
        [ `Annotation
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
        | `Url ]
    type xword =
      Typedefs.xword = {
      rows : int;
      cols : int;
      grid : square array array;
      clues : clues;
      mutable metadata : (metadata_key * string) list;
    }
    type file = Typedefs.file = { name : string; format : string; }
    type converter_input =
      Typedefs.converter_input = {
      data : string;
      input_format : string;
      output_format : string;
    }
    exception PuzzleFormatError of string
    module type READER = sig val read : string -> xword end
    module type WRITER = sig val write : xword -> string end
  end
val make : int -> int -> Typedefs.xword
val get : Typedefs.xword -> int -> int -> Typedefs.square
val set : Typedefs.xword -> int -> int -> Typedefs.square -> unit
val get_cell : Typedefs.xword -> int -> int -> Typedefs.cell
val set_cell : Typedefs.xword -> int -> int -> Typedefs.cell -> unit
val get_num : Typedefs.xword -> int -> int -> int
val set_num : Typedefs.xword -> int -> int -> int -> unit
val is_black : Typedefs.xword -> int -> int -> bool
val boundary : Typedefs.xword -> int -> int -> bool
val non_boundary : Typedefs.xword -> int -> int -> bool
val start_across : Typedefs.xword -> int -> int -> bool
val start_down : Typedefs.xword -> int -> int -> bool
val iteri :
  Typedefs.xword -> (int -> int -> int -> Typedefs.cell -> 'a) -> unit
val renumber :
  ?on_ac:(int -> unit) -> ?on_dn:(int -> unit) -> Typedefs.xword -> unit
val word_ac : Typedefs.xword -> int -> int -> (int * int) list
val word_dn : Typedefs.xword -> int -> int -> (int * int) list
val encode_rebus :
  Typedefs.xword -> (int * string) list * Typedefs.rebus Typedefs.SMap.t
val get_clues : Typedefs.xword -> Typedefs.word_direction -> string list
val clue_numbers : Typedefs.xword -> int list * int list
val format_grid :
  Typedefs.xword ->
  fmt:(Typedefs.cell -> string) -> rowsep:string -> charsep:string -> string
val inspect_grid : Typedefs.xword -> unit
val inspect_clues : Typedefs.xword -> unit
val inspect : Typedefs.xword -> unit
val toggle_black : Typedefs.xword -> int -> int -> bool
val delete_letter : Typedefs.xword -> int -> int -> bool
val set_letter : Typedefs.xword -> int -> int -> Typedefs.letter -> bool
val metadata : Typedefs.xword -> Typedefs.metadata_key -> string
val set_metadata : Typedefs.xword -> Typedefs.metadata_key -> string -> unit
