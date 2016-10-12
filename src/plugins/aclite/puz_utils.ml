open Puz_types
open Types
open Utils

(* CRC checksum for binary format *)
class checksum ~seed =
  object(self)
    val mutable sum = seed

    method sum = sum

    method add_char b =
      let low = sum land 0x001 in
      sum <- sum lsr 1;
      if low = 1 then sum <- sum lor 0x8000;
      sum <- (sum + (Char.code b)) land 0xffff

    method add_string s =
      String.iter self#add_char s

    method add_string_0 s =
      if not (is_empty_string s) then begin
        self#add_string s;
        self#add_char '\000'
      end
  end

let checksum_of_string s =
  let c = new checksum 0 in
  c#add_string s;
  c#sum

(* Access a string as an input stream *)
class string_io _string =
  object
    val str = _string
    val mutable pos = 0

    method remaining = String.length str - pos

    method read n =
      pos <- pos + n;
      String.sub str (pos - n) n

    method read_string =
      try
        let i = String.index_from str pos '\000' in
        let s = String.sub str pos (i - pos) in
        pos <- i + 1;
        s
      with
      | Not_found -> raise (PuzzleFormatError "Could not read string")

  end

(* binary string functions *)

let s0 s = s ^ "\000"

let pad0 s len = s ^ (String.make (len - (String.length s)) '\000')

let array_of_string s = CCString.to_array s

let int32_of_char c = Char.code c |> Int32.of_int

let concat_map ?sep:(sep="") ~f xs = String.concat sep (List.map f xs)
