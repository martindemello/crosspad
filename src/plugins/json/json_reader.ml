open Printf
open Typedefs
open Utils

open Yojson.Safe

(*
 * cell = { x : int, y : int, contents : string }
 *
 * xword = { rows : int, 
 *           cols : int, 
 *           cells : [cell],
 *           across : [string]
 *           down : [string]
 *         }
 *)

(* WRITING *)

let string_of_cell = function
  | Black -> "#"
  | Empty -> "."
  | Letter c -> c
  | Rebus r -> r.solution

let json_cell x y cell =
  `Assoc ["x", `Int x; "y", `Int y; "contents", `String (string_of_cell cell)]

let json_cell_list xw =
  let out = ref [] in
  for y = 0 to xw.rows - 1 do
    for x = 0 to xw.cols - 1 do
      let c = Xword.get_cell xw x y in
      let j = json_cell x y c in
      out := j :: !out
    done
  done;
  List.rev !out

let json_grid xw =
  let cells = json_cell_list xw in
  let str xs = List.map (fun x -> `String x) xs in
  `Assoc [
    "rows", `Int xw.rows;
    "cols", `Int xw.cols;
    "cells", `List cells;
    "across", `List (str xw.clues.across);
    "down", `List (str xw.clues.down)
  ]

let write xword =
  json_grid xword |> Yojson.Safe.to_string 

(* READING *)

type input_cell = {
  x : int;
  y : int;
  contents : string
}

type input_xword = {
  rows : int;
  cols : int;
  cells : input_cell list;
  across : string list;
  down : string list
}

let rebus_of_string s =
  let c = string_of_char s.[0] in
  Rebus { symbol = 0; solution = s; display_char = c }

let cell_of_string s = match s with
  | "#" -> Black
  | "." -> Empty
  | s when (String.length s) = 1 -> Letter s
  | s -> rebus_of_string s

let unpack_cell_fields (js : (string * json) list) =
  let cell = { x = 0; y = 0; contents = "" } in
  let process_field cell (k, v) = match (k, v) with
    | "x", `Int x -> { cell with x = x }
    | "y", `Int y -> { cell with y = y }
    | "contents", `String s -> { cell with contents = s }
    | _ -> raise (PuzzleFormatError "Malformed json")
  in
  List.fold_left process_field cell js

let unpack_cell (j : json) =
  match j with
  | `Assoc js -> unpack_cell_fields js
  | _ -> raise (PuzzleFormatError "Malformed json")

let unpack_clue (j : json) =
  match j with
  | `String s -> s
  | _ -> raise (PuzzleFormatError "Malformed json")

let unpack_toplevel (js : (string * json) list) =
  let xw = { rows = 0; cols = 0; cells = []; across = []; down = [] } in
  let process_field xw (k, v) = match (k, v) with
    | "rows", `Int r -> {xw with rows = r}
    | "cols", `Int c -> {xw with cols = c}
    | "cells", `List cs -> begin
        let cells = List.map unpack_cell cs in
        { xw with cells = cells }
      end
    | "across", `List cs -> begin
        let clues = List.map unpack_clue cs in
        { xw with across = clues }
      end
    | "down", `List cs -> begin
        let clues = List.map unpack_clue cs in
        { xw with down = clues }
      end
    | _ -> raise (PuzzleFormatError "Malformed json")
  in
  List.fold_left process_field xw js

let unpack_json (j : json) =
  match j with
  | `Assoc js -> unpack_toplevel js
  | _ -> raise (PuzzleFormatError "Malformed json")

let to_xword input =
  let xw = Xword.make input.rows input.cols in
  let xw = { xw with clues = { across = input.across; down = input.down } } in
  let set c = Xword.set_cell xw c.x c.y (cell_of_string c.contents) in
  List.iter set input.cells;
  xw

let read data =
  let j = Yojson.Safe.from_string data in
  let input = unpack_json j in
  to_xword input
