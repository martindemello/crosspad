(* Exolve (https://github.com/viresh-ratnakar/exolve) is a browser-based
* crossword solver that allows you to embed the crossword and solving software
* in a single HTML file.
*)

open StdLabels
open Xword.Types


let format_headers xw =
  let header = Xword.metadata xw in
  let headers = [
    "id", "replace-with-unique-id";
    "title", header `Title;
    "setter", header `Author;
    "width", string_of_int xw.rows;
    "height", string_of_int xw.cols;
    "copyright", header `Copyright;
    "prelude", header `Intro 
  ] in
  let fmt (k, v) =
    if v = "" then None else Some ("exolve-" ^ k ^ ": " ^ v)
  in
  CCList.filter_map fmt headers


let format_grid xw ~fmt =
  Xword.format_grid xw ~fmt ~charsep:"" ~rowsep:"\n"
  |> CCString.lines
       

let format_clue (num, clue) =
  (string_of_int num) ^ ". " ^ clue


let format_clues clues =
  List.map ~f:format_clue clues


let indent lines =
  List.map ~f:(fun x -> "  " ^ x) lines


let write_xw xw ~fmt =
  let headers = format_headers xw in
  let across = format_clues xw.clues.across in
  let down = format_clues xw.clues.down in
  let grid = format_grid xw ~fmt in
  let body = List.concat [
      headers;
      ["exolve-grid:"];
      indent grid;
      ["exolve-across:"];
      indent across;
      ["exolve-down:"];
      indent down
    ]
  in
  let out = List.concat [
      ["exolve-begin"];
      indent(body);
      ["exolve-end"]
    ] in
  CCString.unlines out


module Filled = struct
  let write xw =
    let fmt = function
      | Rebus r -> r.display_char
      | Letter c -> c
      | Black -> "."
      | Empty -> "0"
    in
    write_xw xw ~fmt
end

  
module Blank = struct
  let write xw =
    let fmt = function
      | Rebus r -> r.display_char
      | Letter _ -> "0"
      | Black -> "."
      | Empty -> "0"
    in
    write_xw xw ~fmt
end
