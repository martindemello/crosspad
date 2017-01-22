open Printf
open Xword.Types
open Utils

let write_solution xw =
  let fmt = function
    | Rebus r -> r.display_char
    | Letter c -> c
    | _ -> "."
  in
  Xword.format_grid xw ~fmt ~charsep:"" ~rowsep:"\n"

let write_clues cs =
  unlines (List.map snd cs)

let write_rebus xw =
  let rtbl, rmap = Xword.encode_rebus xw in
  let f kv =
    let (sym, sol) = kv in
    match smap_find sol rmap with
    | Some r -> Printf.sprintf "%d:%s:%s" sym r.solution r.display_char
    | None -> raise (PuzzleFormatError "Something went wrong in rebus encoding")
  in
  let rs = List.map f rtbl in
  unlines rs

let write xw =
  let meta = Xword.metadata xw in
  let size = Printf.sprintf "%dx%d" xw.rows xw.cols in
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
  let sections = List.filter (fun (k, v) ->
      not (is_empty_string v)) sections in
  let ss = List.map (fun (k, v) ->
      Printf.sprintf "<%s>\n%s" k v) sections in
  (unlines ss) ^ "\n"

let read_version s = function
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

let parse_section s = list_assoc s sections

let match_text_section s = match parse_section s with
  None -> false
  | Some s -> begin
      match s with 
      | `Title | `Author | `Copyright | `Notes -> true
      |_ -> false
    end

let read_section (lines : string list) =
  let [@ ocaml.warning "-8"] header :: rest = lines in
  let section = parse_section header in
  match section with
  | None -> raise (PuzzleFormatError ("Unrecognised section " ^ header))
  | Some s -> (s, rest)

(*
let get_metadata sections =
  let out = ref [] in
  let get_meta s = match parse_section s with
    | Some section -> out := (s, section) :: !out
    | None -> ()
  in
  List.iter ~f:get_meta [`Title; `Author; `Copyright; `Notes];
  out
   *)

let read data =
  let [@ ocaml.warning "-8"] v :: lines = split_lines data in
  let _version = read_version v in
  let sections = list_group lines ~break:(fun x y ->
     is_some (parse_section x))
  in
  let sections = List.map read_section sections in
  sections
