open Core_kernel.Std
open Puz_types
open Mikmatch

RE ltim = bos (digit+ as x : int) "," (digit+ as y : int) '\000' eos;;

RE rtbl_entry = (" " digit | digit{2}) ":" alpha+ ";"
RE rtbl = bos rtbl_entry+ '\000' eos
RE rtbl_capture = (digit+ as symbol : int) ":" (alpha+ as word)

RE text_section_header = "<" (alpha+ as word) ">"

let match_ltim s = match s with
  | RE ltim -> Some (x, y)
  | _ -> None

let match_rtbl s =
  let scan = COLLECTOBJ rtbl_capture in
  match s with
  | RE rtbl -> Some (scan s)
  | _ -> None

let match_text_section s = match s with
  | RE text_section_header -> Some word
  | _ -> None
