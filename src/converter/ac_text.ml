open Xword.Types
open Xword.Utils
open Puz_match

type puzzle = {
  version: int;
  width: int;
  height: int;
  metadata: (metadata_key * string) list;
  grid: string list;
  across: string list;
  down: string list;
  rebus: string
}

let new_puzzle = {
  version = 0;
  width = 0;
  height = 0;
  metadata = [];
  grid = [];
  across = [];
  down = [];
  rebus = "";
}

[@@@ocaml.warning "-34"] (* unused type sections *)
type sections = [
| `Title
| `Author
| `Copyright
| `Notes
| `Size
| `Grid
| `Rebus
| `Across
| `Down
]

let write_solution xw =
  let fmt = function
    | Rebus r -> r.display_char
    | Letter c -> c
    | _ -> "."
  in
  Xword.format_grid xw ~fmt ~charsep:"" ~rowsep:"\n"

let write_clues cs =
  CCString.unlines (List.map snd cs)

let write_rebus xw =
  let rtbl, rmap = Xword.encode_rebus xw in
  let f kv =
    let (sym, sol) = kv in
    match smap_find sol rmap with
    | Some r -> Printf.sprintf "%d:%s:%s" sym r.solution r.display_char
    | None -> raise (PuzzleFormatError "Something went wrong in rebus encoding")
  in
  let rs = List.map f rtbl in
  CCString.unlines rs

let write xw =
  let meta = Xword.metadata xw in
  let size = Printf.sprintf "%dx%d" xw.rows xw.cols in
  let header = "<ACROSS PUZZLE V2>\n" in
  let sections = [
    "TITLE", meta `Title;
    "AUTHOR", meta `Author;
    "COPYRIGHT", meta `Copyright;
    "SIZE", size;
    "GRID", write_solution xw;
    "REBUS", write_rebus xw;
    "ACROSS", write_clues xw.clues.across;
    "DOWN", write_clues xw.clues.down;
    "NOTEPAD", meta `Notes
  ] in
  let sections = List.filter (fun (_, v) ->
      not (is_empty_string v)) sections in
  let ss = List.map (fun (k, v) ->
      Printf.sprintf "<%s>\n%s" k v) sections in
  header ^ (CCString.unlines ss) ^ "\n"

let read_version = function
  | s when s = "<ACROSS PUZZLE>" -> 1
  | s when s = "<ACROSS PUZZLE V2>" -> 2
  | s when s = "<ACROSS DEBUG>" -> 3
  | _ -> raise (PuzzleFormatError "Could not read AcrossLite version header")

let sections = [
  ("TITLE", `Title);
  ("AUTHOR", `Author);
  ("COPYRIGHT", `Copyright);
  ("NOTEPAD", `Notes);
  ("SIZE", `Size);
  ("GRID", `Grid);
  ("REBUS", `Rebus);
  ("ACROSS", `Across);
  ("DOWN", `Down);
]

let read_section (lines : string list) =
  let header, rest = match lines with
    | [] -> raise (InternalError "Internal error in acrosslite text reader")
    | x :: xs -> (x, xs)
  in
  let error = PuzzleFormatError ("Unrecognised section " ^ header) in
  let section = match_text_section header in
  let (s, rest) = match section with
  | None -> raise error
  | Some s -> (s, rest)
  in
  match list_assoc s sections with
  | None -> raise error
  | Some k -> (k, s, rest)

let add_metadata puzzle key contents =
  (*let key = string_of_metadata_key key in*)
  let m = (key, contents) :: puzzle.metadata in
  { puzzle with metadata = m }

let parse_size s =
  match match_text_size s with
  | Some (x, y) -> (x, y)
  | None -> raise (PuzzleFormatError ("Invalid size " ^ s))

let single_line header contents = match contents with
  | [x] -> x
  | _ -> raise (PuzzleFormatError ("Invalid contents of section " ^ header))

let parse_section puzzle (key, header, contents) =
  let single_line = single_line header in
  match key with
  | (`Title | `Author | `Copyright | `Notes) as k ->
    add_metadata puzzle k (single_line contents)
  | `Size -> begin
      let (rows, cols) = parse_size (single_line contents) in
      { puzzle with width = cols ; height = rows }
    end
  | `Grid -> { puzzle with grid = contents }
  | `Rebus -> { puzzle with rebus = single_line contents }
  | `Across -> { puzzle with across = contents }
  | `Down -> { puzzle with down = contents }


(* puzzle -> xword conversion *)

let cell_of_char c = match c with
  | '.' -> Black
  | ' ' -> Empty
  | c  -> Letter (string_of_char c)

let unpack_solution xw puzzle =
  let s = Array.of_list puzzle.grid in
  for y = 0 to xw.rows - 1 do
    for x = 0 to xw.cols - 1 do
      let r = s.(y) in
      let cell = cell_of_char r.[x] in
      Xword.set_cell xw x y cell
    done
  done

let unpack_clues xw puzzle =
  let ac_clues = Array.of_list puzzle.across in
  let dn_clues = Array.of_list puzzle.down in
  let ac = ref [] in
  let dn = ref [] in
  let i_ac = ref 0 in
  let i_dn = ref 0 in
  ignore @@ Xword.renumber
    ~on_ac:(fun _ -> ac := (!i_ac + 1, ac_clues.(!i_ac)) :: !ac; i_ac := !i_ac + 1)
    ~on_dn:(fun _ -> dn := (!i_dn + 1, dn_clues.(!i_dn)) :: !dn; i_dn := !i_dn + 1)
    xw;
  xw.clues.across <- List.rev !ac;
  xw.clues.down <- List.rev !dn

let unpack_metadata xw p =
  xw.Xword.Types.metadata <- p.metadata

let xword_of_puzzle puzzle =
  let xw = Xword.make puzzle.height puzzle.width in
  unpack_solution xw puzzle;
  unpack_clues xw puzzle;
  unpack_metadata xw puzzle;
  xw

let read data =
  let v, lines = match (CCString.lines data) with
    | [] -> raise (PuzzleFormatError "Could not read input")
    | x :: xs -> (x, xs)
  in
  let _version = read_version v in
  let sections = list_group lines ~break:(fun x _ ->
     is_some (match_text_section x))
  in
  let sections = List.map read_section sections in
  let puz = List.fold_left parse_section new_puzzle sections in
  xword_of_puzzle puz
