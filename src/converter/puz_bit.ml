open Puz_types
open Puz_utils

let file_magic = "ACROSS&DOWN"

(* Read header from binary .puz *)
let read_header data start =
  match%bitstring data with
  | preamble [@l start * 8] [@string],
    checksum [@l 2 * 8],
    magic [@l 0xc * 8] [@string],
    checksum_cib [@l 16] [@littleendian],
    checksum_low [@l 32] [@littleendian],
    checksum_high [@l 32] [@littleendian],
    version [@l 32] [@string],
    reserved1c [@l 16],
    scrambled_checksum [@l 16] [@littleendian],
    reserved20 [@l 0xc * 8] [@string],
    width [@l 8],
    height [@l 8],
    n_clues [@l 16] [@littleendian],
    puzzle_type [@l 16] [@littleendian],
    scrambled_tag [@l 16] [@littleendian] ->
     { new_puzzle with preamble; width; height; version; n_clues }
