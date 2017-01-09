module SMap :
  sig
    type key = String.t
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
type letter = string
type rebus = { symbol : int; solution : string; display_char : string; }
type cell = Black | Empty | Letter of letter | Rebus of rebus
type square = { cell : cell; num : int; }
type clues = { mutable across : string list; mutable down : string list; }
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
type xword = {
  rows : int;
  cols : int;
  grid : square array array;
  clues : clues;
  mutable metadata : (metadata_key * string) list;
}
type file = { name : string; format : string; }
type converter_input = {
  data : string;
  input_format : string;
  output_format : string;
}
exception PuzzleFormatError of string
module type READER = sig val read : string -> xword end
module type WRITER = sig val write : xword -> string end
