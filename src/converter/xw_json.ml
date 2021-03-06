open Xword.Types
open Xword.Utils

open Ezjsonm

(*
 * cell = { x : int, y : int, contents : string }
 *
 * xword = { rows : int,
 *           cols : int,
 *           cells : [cell],
 *           across : [[int, string]]
 *           down : [[int, string]]
 *           metadata : [[key, value]]
 *         }
 *)

(* WRITING *)

let id (x : value) = x

let string_of_cell = function
  | Black -> "#"
  | Empty -> "."
  | Letter c -> c
  | Rebus r -> r.solution

let json_cell x y cell =
  dict ["x", int x; "y", int y; "contents", string (string_of_cell cell)]

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

let write_json xw =
  let cells = json_cell_list xw in
  let clue (n, s) = `A [int n; string s] in
  let md (k, v) = `A [string (string_of_metadata_key k); string v] in
  dict [
    "rows", int xw.rows;
    "cols", int xw.cols;
    "cells", list id cells;
    "across", list clue xw.clues.across;
    "down", list clue xw.clues.down;
    "metadata", list md xw.metadata
  ]

let error err =
  let open Ezjsonm in
  dict [ "error", (string err) ]

let write xword =
  write_json xword |> to_string

(* Reader separated into json_reader.ml because it uses Yojson rather than Jsonm *)
let read data = Json_reader.read data
