(* QXW rectangular grid format *)
open Xword.Types
open StdLabels

let parts prefix line =
  match CCString.chop_prefix ~pre:prefix line with
  | None -> raise (PuzzleFormatError "Internal error in qxw plugin")
  | Some line -> CCString.Split.list_cpy ~by:" " ~drop:{first=true; last=true} line


let lines_with_prefix prefix lines =
  List.find_all ~f:(CCString.prefix ~pre:prefix) lines


let get_gp lines =
  let params = match lines_with_prefix "GP " lines with
  | [] -> raise (PuzzleFormatError "Could not find grid parameters")
  | h :: _ -> parts "GP " h
  in
  match List.map ~f:int_of_string params with
  | 0 :: w :: h :: _ -> (w, h)
  | _ -> raise (PuzzleFormatError "Only rectangular grids are supported")


let read_sq line =
  let ps = parts "SQ " line |> CCArray.of_list in
  let n = CCArray.length ps in
  if n < 5 || n > 6 then begin
    let err = "Error reading grid from qxw file: " ^ line in
    raise (PuzzleFormatError err)
  end;
  let col = int_of_string ps.(0) in
  let row = int_of_string ps.(1) in
  let b = int_of_string ps.(4) in
  let c = CCArray.get_safe ps 5 in
  let cell = match b, c with
  | 1, _ -> Black
  | _, None -> Empty
  | _, Some c -> Letter c
  in
  (row, col, cell)


let read_sqct line =
  let ps = parts "SQCT " line |> CCArray.of_list in
  let n = CCArray.length ps in
  if n < 3 || n > 4 then begin
    let err = "Error reading grid from qxw file: " ^ line in
    raise (PuzzleFormatError err)
  end;
  let col = int_of_string ps.(0) in
  let row = int_of_string ps.(1) in
  let b = int_of_string ps.(2) in
  let c = CCArray.get_safe ps 3 |> CCOption.map (CCString.replace ~sub:"\"" ~by:"") in
  match b with
  | 1 -> None
  | _ -> (
    let cell = match c with
    | None -> Empty
    | Some "." -> Black
    | Some c -> Letter c
    in
    Some (row, col, cell)
  )


let read_cells xw lines =
  let sq = lines_with_prefix "SQCT " lines in
  List.iter sq ~f:(fun line ->
      match read_sqct line with
      | None -> ()
      | Some (row, col, cell) -> Xword.set_cell xw row col cell)


let fill_clues xw =
  let ac = ref [] in
  let dn = ref [] in
  ignore @@ Xword.renumber
    ~on_ac:(fun (x, y, n) -> ac := (n, Xword.word_ac xw x y) :: !ac)
    ~on_dn:(fun (x, y, n) -> dn := (n, Xword.word_dn xw x y) :: !dn)
    xw;
  xw.clues.across <- List.rev !ac;
  xw.clues.down <- List.rev !dn


let read data =
  let lines = CCString.lines data in
  let w, h = get_gp lines in
  let xw = Xword.make h w in
  read_cells xw lines;
  fill_clues xw;
  xw
