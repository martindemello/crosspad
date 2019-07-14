(* Simple text format *)

open Xword.Types

let format_grid xw =
  let fmt = function
    | Rebus r -> r.display_char
    | Letter c -> c
    | Black -> "#"
    | Empty -> "."
  in
  Xword.format_grid xw ~fmt ~charsep:"" ~rowsep:"\n"

let format_clue (num, clue) =
  "  " ^ (string_of_int num) ^ ". " ^ clue

let format_clues ?(format_clue=format_clue) xw =
  let ac = "Across" :: "" :: CCList.map format_clue xw.clues.across in
  let dn = "Down" :: "" :: CCList.map format_clue xw.clues.down in
  CCString.unlines CCList.(ac @ [""] @ dn)

let write xw =
  (format_grid xw) ^ "\n\n" ^ (format_clues xw)

let read data =
  let xw = Xword.make 15 15 in
  let _ = CCString.lines data in
  xw
