open Puz_types
open Puz_utils

let file_magic = "ACROSS&DOWN"

(* Read header from binary .puz *)
let read_header data start =
  let s = Bitstring.bitstring_of_string data in
  bitmatch s with
  | {
      preamble: start * 8 : string;
      checksum: 2 * 8;
      magic: 0xc * 8 : string;
      checksum_cib: 16 : littleendian;
      checksum_low: 32 : littleendian;
      checksum_high: 32 : littleendian;
      version: 32 : string;
      reserved1c : 16;
      scrambled_checksum : 16 : littleendian;
      reserved20 : 0xc * 8 : string;
      width : 8;
      height: 8;
      n_clues: 16 : littleendian;
      puzzle_type: 16 : littleendian;
      scrambled_tag : 16 : littleendian
  } ->
    { new_puzzle with preamble; width; height; version; n_clues }


(* read in extensions *)
let read_extensions (s : string_io) =
  let read_extension_header data =
    let s = Bitstring.bitstring_of_string data in
    bitmatch s with
    | {
        section: 4 * 8 : string;
        length: 16 : littleendian;
        checksum: 16 : littleendian
    } -> { data = ""; section; length; checksum }
  in

  let read_extension s =
    let header = s#read 8 in
    let ex = read_extension_header header in
    let data = s#read (ex.length + 1) in
    { ex with data = data }
  in

  let out = ref [] in
  while s#remaining > 8 do
    out := read_extension s :: !out
  done;
  List.rev !out

(* Write out extensions *)
let write_extension_header e =
  let b = BITSTRING {
    e.section : 4 * 8 : string;
    e.length : 16 : littleendian;
    e.checksum : 16 : littleendian
  } in
  Bitstring.string_of_bitstring b

(* Write out puzzle *)
let header_checksum p =
  let scrambled_tag = 0 in
  let puzzle_type = 0x0001 in
  let b = BITSTRING {
      p.width : 8;
      p.height : 8;
      p.n_clues: 16 : littleendian;
      puzzle_type: 16 : littleendian;
      scrambled_tag : 16 : littleendian
    }
  in
  let s = Bitstring.string_of_bitstring b in
  checksum_of_string s

let text_checksum p seed =
  let c = new checksum seed in
  c#add_string_0 p.title;
  c#add_string_0 p.author;
  c#add_string_0 p.copyright;
  List.iter (fun x -> c#add_string x) p.clues;
  if (p.version = "1.3") then c#add_string_0 p.notes;
  c#sum

let global_checksum p =
  let hc = header_checksum p in
  let c = new checksum hc in
  c#add_string p.solution;
  c#add_string p.fill;
  c#sum

let magic_checksums p =
  let mask = Array.map int32_of_char (array_of_string "ICHEATED") in
  let sums = List.map Int32.of_int [
    text_checksum p 0;
    checksum_of_string p.fill;
    checksum_of_string p.solution;
    header_checksum p
  ] in
  let l, h = ref 0l, ref 0l in
  let (@<), (@>) = Int32.(shift_left, shift_right) in
  let (@|), (@&), (@^) = Int32.(logor, logand, logxor) in
  List.iteri (fun i x ->
      l := (!l @< 8) @| (mask.(3 - i) @^ (x @& 0xffl));
      h := (!h @< 8) @| (mask.(7 - i) @^ (x @> 8));
    ) sums;
  (!l, !h)

type checksums = {
  low : int32; high : int32; cib : int; global : int; scrambled : int
}

let checksums p =
  let l, h = magic_checksums p in
  {
    low = l;
    high = h;
    cib = header_checksum p;
    global = global_checksum p;
    scrambled = 0
  }

let write_header p =
  let ck = checksums p in
  let reserved1c = 0 in
  let reserved20 = String.make 12 '\000' in
  let version = pad0 p.version 4 in
  let b = BITSTRING {
      ck.global: 2 * 8;
      s0 file_magic: 0xc * 8 : string;
      ck.cib: 16 : littleendian;
      ck.low: 32 : littleendian;
      ck.high: 32 : littleendian;
      version: 32 : string;
      reserved1c : 16;
      ck.scrambled : 16 : littleendian;
      reserved20 : 0xc * 8 : string;
      p.width : 8;
      p.height: 8;
      p.n_clues: 16 : littleendian;
      p.puzzle_type: 16 : littleendian;
      p.scrambled_tag : 16 : littleendian
  } in
  Bitstring.string_of_bitstring b
