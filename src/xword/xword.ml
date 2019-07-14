open Typedefs
open Utils

module Types = struct
  include Typedefs
end

module Cursor = Cursor

module Utils = Utils

let empty_square = {
  cell = Empty;
  num = 0;
  bar_right = false;
  bar_down = false;
}

let make rows cols =
  {
    rows = rows;
    cols = cols;
    grid = Array.make_matrix rows cols empty_square;
    solution = Array.make_matrix rows cols empty_square;
    clues = { across = []; down = [] };
    metadata = []
  }

(* solution *)
let get_solution xw x y = xw.solution.(y).(x)

let set_solution xw x y s = xw.solution.(y).(x) <- s

(* grid *)
let get xw x y = xw.grid.(y).(x)

let set xw x y s = xw.grid.(y).(x) <- s

let setmap xw x y (f : square -> square) =
  xw.grid.(y).(x) <- f xw.grid.(y).(x)

let get_cell xw x y = (get xw x y).cell

let set_cell xw x y c =
  setmap xw x y (fun s -> { s with cell = c })

let get_num xw x y = (get xw x y).num

let set_num xw x y n =
  setmap xw x y (fun s -> { s with num = n })

let is_black xw x y = (get_cell xw x y) = Black

let not_black xw x y = not (is_black xw x y)

let get_bar_right xw x y = (get xw x y).bar_right

let get_bar_down xw x y = (get xw x y).bar_down

(* in theory a square has four bars, not two, and bars on the boundary
 * should not count *)
let has_bar_left xw x y =
  if x = 0 then
    false
  else
    get_bar_right xw (x - 1) y

let has_bar_right xw x y =
  if x = xw.cols - 1 then
    false
  else
    get_bar_right xw x y

let has_bar_up xw x y =
  if y = 0 then
    false
  else
    get_bar_down xw x (y - 1)

let has_bar_down xw x y =
  if y = xw.rows - 1 then
    false
  else
    get_bar_down xw x y

let toggle_bar_right xw x y =
  setmap xw x y (fun s -> {s with bar_right = not s.bar_right})

let toggle_bar_down xw x y =
  setmap xw x y (fun s -> {s with bar_down = not s.bar_down})

let left_boundary xw x y =
  x <= 0
  || is_black xw (x - 1) y
  || has_bar_left xw x y

let right_boundary xw x y =
  x >= xw.cols - 1
  || is_black xw (x + 1) y
  || has_bar_right xw x y

let top_boundary xw x y =
  y <= 0
  || is_black xw x (y - 1)
  || has_bar_up xw x y

let bottom_boundary xw x y =
  y >= xw.rows - 1
  || is_black xw x (y + 1)
  || has_bar_down xw x y

let boundary dir = match dir with
  | `Left -> left_boundary
  | `Right -> right_boundary
  | `Up -> top_boundary
  | `Down -> bottom_boundary

let start_across xw x y =
  not_black xw x y &&
  left_boundary xw x y &&
  not (right_boundary xw x y)

let start_down xw x y =
  not_black xw x y &&
  top_boundary xw x y &&
  not (bottom_boundary xw x y)

let iteri xw f =
  for y = 0 to xw.rows - 1 do
    for x = 0 to xw.cols - 1 do
      f (x + y * xw.cols) x y (get_cell xw x y)
    done
  done

let renumber ?(on_ac=ignore) ?(on_dn=ignore) xw =
  let n = ref 1 in
  for y = 0 to xw.rows - 1 do
    for x = 0 to xw.cols - 1 do
      let a, d = start_across xw x y, start_down xw x y in
      if a then on_ac (x, y, !n);
      if d then on_dn (x, y, !n);
      if (a || d) then begin
        set_num xw x y !n;
        n := !n + 1;
      end
      else
        set_num xw x y 0;
    done
  done;
  xw

(* collect letters between (x, y) and the boundary in the (dx, dy) direction *)
let rec collect_word xw x y dir out =
  let (dx, dy) = Cursor.delta (dir :> direction) in
  if is_black xw x y then
    out
  else if boundary dir xw x y then
    (x, y) :: out
  else
    collect_word xw (x + dx) (y + dy) dir ((x, y) :: out)

let word_coords_ac xw x y =
  if is_black xw x y then
    []
  else begin
    let out = List.rev @@ collect_word xw x y `Left [] in
    if right_boundary xw x y then
      out
    else
      List.rev @@ collect_word xw (x + 1) y `Right out
  end

let word_coords_dn xw x y =
  if is_black xw x y then
    []
  else begin
    let out = List.rev @@ collect_word xw x y `Up [] in
    if bottom_boundary xw x y then
      out
    else
      List.rev @@ collect_word xw x (y + 1) `Down out
  end

let string_of_cell = function
  | Letter s -> s
  | Rebus r -> r.display_char
  | Black -> "#"
  | Empty -> "."

let string_of_cells cells =
  let letters = List.map string_of_cell cells in
  CCString.concat "" letters

let cells_of_coords xw coords =
  List.map (fun (x, y) -> get_cell xw x y) coords

let word_ac xw x y =
  word_coords_ac xw x y
  |> cells_of_coords xw
  |> string_of_cells

let word_dn xw x y =
  word_coords_dn xw x y
  |> cells_of_coords xw
  |> string_of_cells


(* Update the 'symbol' field in every rebus square, so that cells
 * with the same solution have the same symbol. Symbols are
 * integers from 0..
 *
 * Returns:
 *  a list of [(symbol, solution)]
 *  a map of {solution -> Rebus}
 *)
let encode_rebus xw =
  let m = ref SMap.empty in
  let l = ref [] in
  let k = ref 0 in
  iteri xw (fun _ x y c ->
      match c with
      | Rebus r -> begin
          match smap_find r.solution !m with
          | Some sr -> set_cell xw x y (Rebus sr)
          | None -> begin
              let nr = { r with symbol = !k } in
              k := !k + 1;
              set_cell xw x y (Rebus nr);
              m := SMap.add nr.solution nr !m;
              l := (nr.symbol, nr.solution) :: !l
            end
        end
      | _ -> ()
    );
  (List.rev !l, !m)

let get_clues xw (dir : word_direction) = match dir with
  | `Across -> xw.clues.across
  | `Down -> xw.clues.down

let clue_numbers xw =
  let ac = ref [] in
  let dn = ref [] in
  let _ = renumber
    ~on_ac:(fun n -> ac := n :: !ac)
    ~on_dn:(fun n -> dn := n :: !dn)
    xw
  in
  List.rev !ac, List.rev !dn

let format_grid xw ~fmt ~rowsep ~charsep =
  let s = ref "" in
  for y = 0 to xw.rows - 1 do
    for x = 0 to xw.cols - 1 do
      let c = get_cell xw x y in
      s := !s ^ fmt c ^ charsep
    done;
    if y <> xw.rows - 1 then s := !s ^ rowsep
  done;
  !s

let inspect_grid xw =
  let s = format_grid xw ~rowsep:"\n" ~charsep:" " ~fmt:string_of_cell in
  print_endline s

let inspect_clues xw =
  let print_clue (n, clue) = Printf.printf "%d. %s\n" n clue in
  print_endline "Across";
  List.iter print_clue xw.clues.across;
  print_endline "Down";
  List.iter print_clue xw.clues.down

let inspect xw =
  inspect_grid xw;
  inspect_clues xw

(* Set cell values with constraints:
 * - Do not delete or overwrite a black cell
 * - Do not overwrite a letter with a black cell
 *
 * Returns true if the cell value was changed.
 *
 * Use set_cell to set cells unconditionally *)

let rotate_xy xw x y rot =
  let xmax = xw.cols - 1 in
  let ymax = xw.rows - 1 in
  match rot with
  | R0 -> (x, y)
  | R1 -> (xmax - y, x)
  | R2 -> (xmax - x, ymax - y)
  | R3 -> (y, ymax - x)

let rotate_dir rot =
  match rot with
  | R0 -> fun x -> x
  | R1 -> (function
        `Right -> `Down | `Down -> `Left | `Left -> `Up | `Up -> `Right )
  | R2 -> (function
        `Right -> `Left | `Down -> `Up | `Left -> `Right | `Up -> `Down )
  | R3 -> ( function
        `Right -> `Up | `Down -> `Right | `Left -> `Down | `Up -> `Left )

let rotations = function
  | SymmNone -> [R0]
  | Symm90 -> [R0; R1; R2; R3]
  | Symm180 -> [R0; R2]

(* set a cell and its symmetrical cells to black/empty iff no filled
 * cells are affected *)
let toggle_black ?(symmetry=SymmNone) xw x y =
  let targets = List.map (rotate_xy xw x y) (rotations symmetry) in
  let can_toggle (x, y) = match get_cell xw x y with
    | Black | Empty -> true
    | _ -> false
  in
  if List.for_all can_toggle targets then
    let state = match get_cell xw x y with Black -> Empty | _ -> Black in
    List.iter (fun (x, y) -> set_cell xw x y state) targets;
    true
  else
    false

(* toggle bars, maintaining symmetry *)
let toggle_bar ?(symmetry=SymmNone) xw x y d =
  if (x = 0 && d = `Left) ||
     (x = xw.cols - 1 && d = `Right) ||
     (y = 0 && d = `Up) ||
     (y = xw.rows - 1 && d = `Down) then
    false
  else begin
    let target rot = (rotate_xy xw x y rot), (rotate_dir rot d) in
    let targets = List.map target (rotations symmetry) in
    let toggle ((x, y), d) = match d with
      | `Left -> toggle_bar_right xw (x - 1) y
      | `Right -> toggle_bar_right xw x y
      | `Up -> toggle_bar_down xw x (y - 1)
      | `Down -> toggle_bar_down xw x y
    in
    List.iter toggle targets;
    true
  end

(* delete a cell unless it is black *)
let delete_letter xw x y =
  match get_cell xw x y with
  | Black -> false
  | _ -> set_cell xw x y Empty; true

let set_letter xw x y s =
  match get_cell xw x y with
  | Black -> false
  | _ -> set_cell xw x y (Letter s); true

(* clear fill *)
let clear_fill xw =
  for y = 0 to xw.rows - 1 do
    for x = 0 to xw.cols - 1 do
      ignore @@ delete_letter xw x y
    done
  done;
  xw

(* for solving mode, we load the grid and copy it to the solution *)
let copy_to_solution xw =
  for y = 0 to xw.rows - 1 do
    for x = 0 to xw.cols - 1 do
      set_solution xw x y (get xw x y)
    done
  done;
  xw

let init_solve_mode xw =
  copy_to_solution xw |> clear_fill

(* Get and set metadata *)

let metadata xw k =
  match list_assoc k xw.metadata with
  | Some v -> v
  | None -> ""

let set_metadata xw k v =
  xw.metadata <- (k, v) :: xw.metadata
