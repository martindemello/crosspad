open Core_kernel.Std
open Printf
open Types
open Puz_types
open Puz_utils

let fail_read ex =
  let msg = Printf.sprintf "Could not read extension %s" ex.section in
  raise (PuzzleFormatError msg)

(* extension -> parsed_extension *)
let parse_extension puz ex =
  let n_cells = puz.width * puz.height in
  let last s = String.length s - 1 in
  let is_byte_grid s = (last s = n_cells) && (s.[n_cells] = '\000') in
  let drop_last s = String.sub s 0 (last s) in
  match ex.section with
  | "RTBL" -> begin
      let unpack x = (x#symbol, x#word) in
      match Puz_match.match_rtbl ex.data with
      | None -> fail_read ex
      | Some xs -> ("RTBL", `RTBL (List.map xs unpack))
    end
  | "GRBS" -> begin
      match is_byte_grid ex.data with
      | false -> fail_read ex
      | true -> ("GRBS", `GRBS (drop_last ex.data))
    end
  | "GEXT" -> begin
      match is_byte_grid ex.data with
      | false -> fail_read ex
      | true -> ("GEXT", `GEXT (drop_last ex.data))
    end
  | "LTIM" -> begin
      match Puz_match.match_ltim ex.data with
      | None -> fail_read ex
      | Some (x, y) -> ("LTIM", `LTIM (x, y))
    end
  |_ -> fail_read ex

(* parsed_extension -> extension *)
let write_ltim (x, y) =
  Printf.sprintf "%d,%d" x y

let write_grbs s = s

let write_gext s = s

let write_rtbl xs =
  String.concat (List.map xs (fun (x, y) -> Printf.sprintf "%2d:%s;" x y))

let data_of_extension = function
  | `RTBL x -> write_rtbl x
  | `GRBS x -> write_grbs x
  | `GEXT x -> write_gext x
  | `LTIM x -> write_ltim x

let pack_extension (section, p) =
  let data = data_of_extension p in
  let length = String.length data in
  let data = data ^ "\000" in
  let checksum = checksum_of_string data in
  { section; data; length; checksum }

let write_extension ex =
  let h = Puz_bin.write_extension_header ex in
  h ^ ex.data

(* puzzle -> data *)
let write_puzzle p =
  let s0 s = s ^ "\000" in
  Puz_bin.write_header p ^
  p.solution ^
  p.fill ^
  s0 p.title ^
  s0 p.author ^
  s0 p.copyright ^
  (String.concat ~sep:"" (List.map p.clues s0)) ^
  p.notes ^
  String.concat ~sep:"" (List.map p.extensions write_extension)

(* data -> puzzle *)
let read_puzzle data =
  (* Files may contain some data before the start of the puzzle.
     Use the magic string as a start marker and save the preamble for
     round-tripping *)
  let start =
    let file_magic_rx = Str.regexp_string Puz_bin.file_magic in
    try (Str.search_forward file_magic_rx data 0) - 2
    with Not_found -> raise (PuzzleFormatError "Could not find start of puzzle")
  in
  let s = new string_io data in
  let header = s#read (start + 0x34) in
  let puz = Puz_bin.read_header header start in
  let solution = s#read (puz.width * puz.height) in
  let fill = s#read (puz.width * puz.height) in
  let title = s#read_string in
  let author = s#read_string in
  let copyright = s#read_string in
  let clues = Array.init puz.n_clues (fun i -> s#read_string) in
  let notes = s#read_string in
  let extensions = Puz_bin.read_extensions s in
  { puz with solution; fill; title; author; copyright; notes;
             extensions; clues = Array.to_list clues
  }

(* puzzle -> xword conversion *)
let cell_of_char c = match c with
  | '.' -> Black
  | ' ' -> Empty
  | c  -> Letter (Char.to_string c)

let unpack_clues xw puzzle =
  let clues = Array.of_list puzzle.clues in
  let ac = ref [] in
  let dn = ref [] in
  let i = ref 0 in
  Xword.renumber
    ~on_ac:(fun n -> ac := clues.(!i) :: !ac; i := !i + 1)
    ~on_dn:(fun n -> dn := clues.(!i) :: !dn; i := !i + 1)
    xw;
  xw.clues.across <- List.rev !ac;
  xw.clues.down <- List.rev !dn

let unpack_solution xw puzzle =
  let s = puzzle.solution in
  for y = 0 to xw.rows - 1 do
    for x = 0 to xw.cols - 1 do
      let ix = y * xw.cols + x in
      let cell = cell_of_char s.[ix] in
      Xword.set_cell xw x y cell
    done
  done

let unpack_rebus xw grbs rtbl =
  Xword.iteri xw (fun i x y c ->
      let n = Char.to_int grbs.[i] in
      if n > 0 then begin
        let n = n - 1 in
        let s = match List.Assoc.find rtbl n with
          | Some str -> str
          | None -> raise (PuzzleFormatError "Invalid rebus extension")
        in
        let display_char = String.sub s ~pos:0 ~len:1 in
        let r = Rebus { symbol = n; solution = s; display_char } in
        Xword.set_cell xw x y r
      end
    )

let unpack_extensions xw puzzle =
  let ex = List.map puzzle.extensions ~f:(parse_extension puzzle) in
  let get_ex = List.Assoc.find ex in
  match get_ex "GRBS", get_ex "RTBL" with
  | Some (`GRBS grbs), Some (`RTBL rtbl) -> unpack_rebus xw grbs rtbl
  | _ -> () (* if we don't have both rtbl and grbs do nothing *)

let unpack_metadata xw p =
  xw.metadata <- [
    ("Title", p.title);
    ("Author", p.author);
    ("Copyright", p.copyright);
    ("Notes", p.notes)
  ]

let to_xw puzzle =
  let xw = Xword.make puzzle.height puzzle.width in
  unpack_solution xw puzzle;
  unpack_clues xw puzzle;
  unpack_extensions xw puzzle;
  unpack_metadata xw puzzle;
  xw

(* xword -> puzzle conversion *)
let read data =
  let puz = read_puzzle data in
  let xw = to_xw puz in
  xw

let pack_clues xw =
  let cmp (x, _) (y, _) = compare x y in
  let hd, tl = List.hd_exn, List.tl_exn in
  let ac, dn = Xword.clue_numbers xw in
  let nums = List.append
      (List.map ~f:(fun x -> (x, `Ac)) ac)
      (List.map ~f:(fun x -> (x, `Dn)) dn)
  in
  let nums = List.sort ~cmp nums in
  let rec weave ns a d out = match ns with
    | [] -> List.rev out
    | (x, `Ac) :: xs -> weave xs (tl a) d ((hd a) :: out)
    | (x, `Dn) :: xs -> weave xs a (tl d) ((hd d) :: out)
  in
  weave nums xw.clues.across xw.clues.down []

(* pack a grid as a single string of chars *)
let pack_grid xw ~fmt = Xword.format_grid xw ~charsep:"" ~rowsep:"" ~fmt

let pack_extensions xw =
  let rtbl = Xword.encode_rebus xw in
  let fmt = function
    | Rebus r -> Char.to_string (Char.of_int_exn r.symbol)
    | _ -> "\000"
  in
  let grbs = pack_grid xw ~fmt in
  List.map ~f:pack_extension ["GRBS", `GRBS grbs; "RTBL", `RTBL rtbl]

let pack_solution xw =
  let fmt = function
    | Rebus r -> r.display_char
    | Letter c -> c
    | _ -> "."
  in
  pack_grid xw ~fmt

let to_puzzle xw =
  let meta = Xword.metadata xw in
  let clues = pack_clues xw in
  let empty_fill = pack_grid xw ~fmt:(function Black -> "." | _ -> "-") in
  { new_puzzle with
    title = meta "Title";
    author = meta "Author";
    copyright = meta "Copyright";
    width = xw.cols;
    height = xw.rows;
    n_clues = List.length clues;
    fill = empty_fill;
    solution = pack_solution xw;
    clues = clues;
    extensions = pack_extensions xw;
  }
 

(* Writer *)
let write xword =
  let puz = to_puzzle xword in
  write_puzzle puz

let _ =
  let fname = "mini.puz" in
  let data = In_channel.read_all fname in
  read data
