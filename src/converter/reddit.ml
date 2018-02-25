(* Markup used by crosswords.reddit.com *)

open Typedefs
open Utils

let format_grid xw ~filled =
  let format_square sq =
    let format_number = function 
      | 0 -> ""
      | n -> "^" ^ string_of_int n
    in
    let format_letter s =
      let s = if filled then s else "" in
      (CCString.uppercase_ascii s) ^ (format_number sq.num)
    in
    match sq.cell with
    | Black -> "*.*"
    | Empty -> " "
    | Letter x -> format_letter x
    | Rebus r -> format_letter r.display_char
  in
  let format_line cells = "|" ^ (CCString.concat "|" cells) ^ "|" in
  let format_row row = CCList.map format_square row |> format_line in
  let grid = lists_of_grid xw in
  let rows = CCList.map format_row grid in
  let sep = format_line (CCList.init xw.cols (fun x -> "--")) in
  let out = CCList.((hd rows) :: sep :: (tl rows)) in
  CCString.unlines out

let format_clues xw =
  let format_clue (num, clue) =
    "  " ^ (string_of_int num) ^ "\\. " ^ clue ^ "  "
  in
  let ac = "Across" :: "" :: CCList.map format_clue xw.clues.across in
  let dn = "Down" :: "" :: CCList.map format_clue xw.clues.down in
  CCString.unlines CCList.(ac @ [""] @ dn)

module Filled = struct
  let write xw =
    (format_grid ~filled:true xw) ^ "\n\n" ^ (format_clues xw)
end

module Blank = struct
  let write xw =
    (format_grid ~filled:false xw) ^ "\n\n" ^ (format_clues xw)
end
