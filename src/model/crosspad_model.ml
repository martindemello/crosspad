open Xword.Types

module Cursor = Xword.Cursor

(* Editable clue with a field for notes *)
type working_clue = {
  number : int;
  initial_clue : string;
  clue : string;
  notes : string
}

module Coords = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match Pervasives.compare x0 x1 with
      0 -> Pervasives.compare y0 y1
    | c -> c
end

module CSet = Set.Make(Coords)

let set_of_list xs =
  List.fold_left (fun set elem -> CSet.add elem set)
    CSet.empty xs

module Model = struct
  open Cursor

  type t = {
    xw : xword;
    cursor : Cursor.t;
    current_dir : word_direction;
    current_word : CSet.t;
    clues_ac : working_clue list;
    clues_dn : working_clue list;
    current_ac : int;
    current_dn : int;
    symmetry : symmetry;
    grid_locked : bool;
    file : file;
    edit_mode : edit_mode;
    debug : string
  }

  let set_debug s model =
    { model with debug = s }

  (* updates *)
  let renumber model =
    let _ = Xword.renumber model.xw in
    model

  let update_current_word model =
    let get_word = match model.current_dir with
      | `Across -> Xword.word_ac
      | `Down -> Xword.word_dn
    in
    let c = get_word model.xw model.cursor.x model.cursor.y in
    let s = set_of_list c in
    { model with current_word = s }

  (* cursor *)
  let set_current_dir d model =
    { model with current_dir = d }

  let toggle_current_dir model =
    let d = match model.current_dir with `Across -> `Down | `Down -> `Across in
    { model with current_dir = d }
    |> update_current_word

  let set_cursor x y model =
    if x = model.cursor.x && y = model.cursor.y then
      model |> toggle_current_dir
    else
      let cursor' = Cursor.set model.cursor x y in
      { model with cursor = cursor' }
      |> update_current_word

  let move_cursor ?wrap:(wrap=true) (d : direction) model =
    let cursor' = Cursor.move model.cursor ~wrap d in
    { model with cursor = cursor' }

  let advance_cursor model =
    move_cursor ~wrap:false (model.current_dir :> direction) model

  let backspace_cursor model =
    let dir' = match model.current_dir with
      | `Across -> `Bksp_Ac | `Down -> `Bksp_Dn
    in
    move_cursor ~wrap:false dir' model

  let movement_key (d : grid_direction) model =
    let dir' : word_direction = match d with
      | `Left | `Right -> `Across
      | `Up | `Down -> `Down
    in
    model
    |> move_cursor ~wrap:true (d :> direction)
    |> set_current_dir dir'
    |> update_current_word

  (* clues *)
  let set_current_clue d n model =
    match d with
    | `Across -> { model with current_ac = n }
    | `Down -> { model with current_dn = n }

  (* grid *)

  (* set a letter only if the current square is white *)
  let set_current_letter s model =
    let xw = model.xw in
    let x, y = model.cursor.x, model.cursor.y in
    let _ = match s with
    | Some s -> Xword.set_letter xw x y s
    | None -> Xword.delete_letter xw x y
    in
    model

  let toggle_black model =
    let xw = model.xw in
    let x, y = model.cursor.x, model.cursor.y in
    if Xword.toggle_black ~symmetry:model.symmetry xw x y then
      ignore @@ Xword.renumber xw;
    model
    |> advance_cursor
    |> update_current_word

  let toggle_bar model (d : grid_direction) =
    let xw = model.xw in
    let x, y = model.cursor.x, model.cursor.y in
    if Xword.toggle_bar ~symmetry:Symm180 xw x y d then
      model
      |> renumber
      |> update_current_word
    else
      model

  let set_letter s model =
    model
    |> set_current_letter (Some s)
    |> advance_cursor

  let delete_letter model =
    model
    |> set_current_letter None

  let backspace_letter model =
    model
    |> backspace_cursor
    |> delete_letter

  (* init *)
  let init_clue (number, initial_clue) =
    { number; initial_clue; clue = initial_clue; notes = "Notes" }

  let init xw =
    { xw;
      cursor = Cursor.make xw.rows xw.cols;
      current_dir = `Across;
      current_word = CSet.empty;
      clues_ac = List.map init_clue xw.clues.across;
      clues_dn = List.map init_clue xw.clues.down;
      current_ac = 0;
      current_dn = 0;
      symmetry = Symm180;
      grid_locked = false;
      file = { name = ""; format = ""};
      edit_mode = `Edit;
      debug = ""
    }
    |> renumber
    |> update_current_word

  let new_xw rows cols =
    let xw = Xword.make rows cols in
    let clues = {
      across = [];
      down = []
    }
    in
    let xw = { xw with clues } in
    init xw
end

module Action = struct
  type action =
    | SetCursor of int * int
    | MoveCursor of grid_direction
    | SetDirection of word_direction
    | ToggleDir
    | SetLetter of string
    | ToggleBlack
    | ToggleBar of grid_direction
    | Backspace
    | Delete
    | SetClue of word_direction * int
    | SetDebug of string
    | Nothing
end

module Controller = struct
  let update action (model, f) =
    let open Action in
    let open Model in
    let model =
      match action with
      | SetCursor (x, y) -> set_cursor x y model
      | MoveCursor d -> movement_key d model
      | SetDirection d -> set_current_dir d model
      | ToggleDir -> toggle_current_dir model
      | ToggleBlack -> toggle_black model
      | ToggleBar d -> toggle_bar model d
      | SetLetter s -> set_letter s model
      | Backspace -> backspace_letter model
      | Delete -> delete_letter model
      | SetClue (d, n) -> set_current_clue d n model
      | SetDebug s -> { model with debug = s }
      | Nothing -> model
    in
    f model
end

module Presenter = struct
  open Model
  open Cursor

  type background_color =
    [ `Black
    | `White
    | `CursorBlack
    | `CursorWhite
    | `CursorSymmBlack
    | `CursorSymmWhite
    | `CurrentWord
    ]

  (* Status *)

  let display_symmetry = function
    | SymmNone -> "None"
    | Symm90 -> "90°"
    | Symm180 -> "180°"

  let display_locked = function
    | true -> "Locked"
    | false -> "Unlocked"

  (* Grid display *)

  let letter_of_cell = function
    | Letter s -> s
    | Rebus r -> r.display_char
    | _ -> ""

  let display_num = function
    | 0 -> ""
    | n -> string_of_int n

  let cell_background x y model =
    let cell = Xword.get_cell model.xw x y in
    let is_cur = model.cursor.x = x && model.cursor.y = y in
    let is_word = CSet.mem (x, y) model.current_word in
    let bg =
      if is_cur then match cell with
        | Black -> `CursorBlack
        | _ -> `CursorWhite
      else if is_word then
        `CurrentWord
      else match cell with
        | Black -> `Black
        | _ -> `White
    in
    bg

  (* Input *)

  let letter_of_code k =
    if k >= 65 && k <= 90 then
      Some (String.make 1 @@ Char.chr k)
    else if k >= 97 && k <= 122 then
      Some (String.make 1 @@ Char.chr (k - 32))
    else
      None

  (* Grid accessors *)

  let square x y model =
    Xword.get model.xw x y

  let letter x y model =
    letter_of_cell (square x y model).cell

  let number x y model =
    display_num (square x y model).num

  (* Clues *)

  let cluebox_header dir = match dir with
    | `Across -> "Across"
    | `Down -> "Down"

  let clue_list dir model = match dir with
    | `Across -> model.clues_ac
    | `Down -> model.clues_dn

  let current_clue dir model = match dir with
    | `Across -> model.current_ac
    | `Down -> model.current_dn

  let format_clue (n, s) =
    Printf.sprintf "%d. %s" n s

end
