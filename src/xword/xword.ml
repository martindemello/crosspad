open Typedefs
open Utils

module Types = struct
  include Typedefs
end

let make rows cols =
  let sq = { cell = Empty; num = 0 } in
  {
    rows = rows;
    cols = cols;
    grid = Array.make_matrix rows cols sq;
    clues = { across = []; down = [] };
    metadata = []
  }

let get xw x y = xw.grid.(y).(x)

let set xw x y s = xw.grid.(y).(x) <- s

let get_cell xw x y = (get xw x y).cell

let set_cell xw x y c = xw.grid.(y).(x) <- { xw.grid.(y).(x) with cell = c }

let get_num xw x y = (get xw x y).num

let set_num xw x y n = xw.grid.(y).(x) <- { xw.grid.(y).(x) with num = n }

let is_black xw x y = (get_cell xw x y) = Black

let boundary xw x y =
  (x < 0) || (y < 0) ||
  (x >= xw.cols) || (y >= xw.rows) ||
  is_black xw x y

let non_boundary xw x y = not (boundary xw x y)

let start_across xw x y =
  (boundary xw (x - 1) y) &&
  (non_boundary xw x y) &&
  (non_boundary xw (x + 1) y)

let start_down xw x y =
  (boundary xw x (y - 1)) &&
  (non_boundary xw x y) &&
  (non_boundary xw x (y + 1))

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
      if a then on_ac !n;
      if d then on_dn !n;
      if (a || d) then begin
        set_num xw x y !n;
        n := !n + 1;
      end
      else
        set_num xw x y 0;
    done
  done

(* collect letters between (x, y) and the boundary in the (dx, dy) direction *)
let rec collect_word xw x y dx dy out =
  if boundary xw x y then
    out
  else
    collect_word xw (x + dx) (y + dy) dx dy ((x, y) :: out)

let word_ac xw x y =
  let out = collect_word xw x y (-1) 0 [] in
  collect_word xw (x + 1) y 1 0 (List.rev out)

let word_dn xw x y =
  let out = collect_word xw x y 0 (-1) [] in
  collect_word xw x (y + 1) 0 1 (List.rev out)

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
  iteri xw (fun i x y c ->
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
  renumber
    ~on_ac:(fun n -> ac := n :: !ac)
    ~on_dn:(fun n -> dn := n :: !dn)
    xw;
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
  let fmt = function
    | Black -> "#"
    | Empty -> "."
    | Letter c -> c
    | Rebus r -> r.display_char
  in
  let s = format_grid xw ~rowsep:"\n" ~charsep:" " ~fmt in
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

let symm_180 xw x y =
  let xmax = xw.cols - 1 in
  let ymax = xw.rows - 1 in
  [ (x, y); (xmax - x, ymax - y) ]

let symm_90 xw x y =
  let xmax = xw.cols - 1 in
  let ymax = xw.rows - 1 in
  [ (x, y);
    (xmax - x, ymax - y);
    (xmax - y, x);
    (y, ymax - x);
  ]

let target_cells x y xw symmetry =
  match symmetry with
  | `SymmNone -> [x, y]
  | `Symm90 -> symm_90 xw x y
  | `Symm180 -> symm_180 xw x y

(* set a cell and its symmetrical cells to black/empty iff no filled
 * cells are affected *)
let toggle_black ?(symmetry=`SymmNone) xw x y =
  let targets = target_cells x y xw symmetry in
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

(* delete a cell unless it is black *)
let delete_letter xw x y =
  match get_cell xw x y with
  | Black -> false
  | _ -> set_cell xw x y Empty; true

let set_letter xw x y s =
  match get_cell xw x y with
  | Black -> false
  | _ -> set_cell xw x y (Letter s); true

(* Get and set metadata *)

let metadata xw k =
  match list_assoc k xw.metadata with
  | Some v -> v
  | None -> ""

let set_metadata xw k v =
  xw.metadata <- (k, v) :: xw.metadata
