open Tk
open StdLabels
open Xword.Types
open Cursor

let top = openTk () ;;
Wm.title_set top "Pangrid" ;;


(* Set up grid on canvas *)
let scale = 30

let topleft x y = x * scale, y * scale

let square_coords x y =
  let x, y = topleft x y in
  x, y, x + scale, y + scale

let letter_coords x y =
  let x, y = topleft x y in
  let s = scale/2 in
  x + s, y + s

let number_coords x y =
  let x, y = topleft x y in
  x + scale - 5, y + 5

let make_square canvas x y =
  let x1, y1, x2, y2 = square_coords x y in
  Canvas.create_rectangle canvas ~x1:x1 ~y1:y1 ~x2:x2 ~y2:y2 ~fill:`White

let make_letter canvas x y =
  let x, y = letter_coords x y in
  Canvas.create_text canvas ~x:x ~y:y ~text:" " ~font:"Arial 14"

let make_number canvas x y =
  let x, y = number_coords x y in
  Canvas.create_text canvas ~x:x ~y:y ~text:" " ~font:"Arial 6"

type xw_cell = {
  square : Tk.tagOrId;
  letter : Tk.tagOrId;
  number : Tk.tagOrId;
}

let make_xword ~canvas ~grid =
  Array.mapi grid ~f:(fun y row ->
      Array.mapi row ~f: (fun x _ -> begin
            let sq = make_square canvas x y
            and l = make_letter canvas x y
            and n = make_number canvas x y
            in { square = sq; letter = l; number = n }
          end))

let printxy x y = print_endline ((string_of_int x) ^ ", " ^ (string_of_int y))

let letter_of_cell = function
  | Black -> ""
  | Empty -> ""
  | Letter c -> c
  | Rebus r -> r.display_char

let bg_of_cell cell is_cursor =
 match cell, is_cursor with
  | Black, false -> `Black
  | Black, true -> `Color "dark green"
  | _, false -> `White
  | _, true -> `Color "pale green"

class xw_canvas ~parent ~xword:xw =
  let width = xw.cols * scale in
  let height = xw.rows * scale in
  let canvas = Canvas.create
      ~width ~height ~borderwidth:2 ~relief:`Sunken ~background: `White
      parent in
  object(self)
  val rows = xw.rows
  val cols = xw.cols
  val cells = make_xword ~canvas:canvas ~grid:xw.grid
  val mutable cursor = Cursor.make xw.rows xw.cols
  val mutable dir : [`Across | `Down] = `Across

  initializer
    self#update_display;
    self#make_bindings;
    pack [canvas]

  method make_bindings =
    (* per-square mouse bindings *)
    for y = 0 to rows - 1 do
      for x = 0 to cols - 1 do begin
        let bind_obj b =
          Canvas.bind canvas b ~events:[`ButtonPress]
            ~action:(fun _ -> self#set_cursor {cursor with x; y})
        in
        let c = cells.(y).(x) in
        (* we need to have all three components register clicks *)
        List.iter ~f:bind_obj [c.square; c.letter; c.number]
      end done
    done;
    (* keyboard bindings *)
    bind canvas ~events:[`KeyPress] ~fields:[`Char; `KeySymString]
      ~action:(fun ev -> self#handle_keypress ev);
    Focus.set canvas

  method update_display =
    Xword.renumber xw |> ignore;
    for y = 0 to rows - 1 do
      for x = 0 to cols - 1 do
        self#sync_cell x y
      done
    done;

  method current_cell =
    Xword.get_cell xw cursor.x cursor.y

  method set_cursor new_cursor =
    let ox, oy = cursor.x, cursor.y in
    cursor <- new_cursor;
    self#sync_bg cursor.x cursor.y;
    self#sync_bg ox oy

  method move_cursor ?wrap:(wrap=true) d =
    let new_cursor = Cursor.move cursor ~wrap:wrap d in
    self#set_cursor new_cursor;

  method toggle_dir = dir <- match dir with
      `Across -> `Down | `Down -> `Across

  method toggle_black =
    if Xword.toggle_black xw cursor.x cursor.y then begin
      Xword.renumber xw |> ignore;
      self#update_display
    end

  method set_letter s =
    Xword.set_cell xw cursor.x cursor.y (Letter s);
    self#sync_cell cursor.x cursor.y;
    self#move_cursor ~wrap:false (dir :> direction)

  method handle_keypress ev =
    match ev.ev_KeySymString with
    | "Left" -> self#move_cursor `Left
    | "Right" -> self#move_cursor `Right
    | "Up" -> self#move_cursor `Up
    | "Down" -> self#move_cursor `Down
    | "space" -> self#toggle_black
    | "Tab" -> self#toggle_dir
    | s when String.length(s) == 1 -> self#set_letter s
    | s -> print_endline s


  method sync_bg x y =
    let cell = Xword.get_cell xw x y in
    let is_cursor = (x, y) = (cursor.x, cursor.y) in
    Canvas.configure_rectangle canvas cells.(y).(x).square
      ~fill:(bg_of_cell cell is_cursor)

  method sync_letter x y =
    let cell = Xword.get_cell xw x y in
    Canvas.configure_text canvas cells.(y).(x).letter
      ~text:(letter_of_cell cell)

  method sync_number x y =
    let n = Xword.get_num xw x y in
    let s = if (n = 0) then " " else (string_of_int n) in
    Canvas.configure_text canvas cells.(y).(x).number ~text:s

  method sync_cell x y =
    self#sync_bg x y;
    self#sync_number x y;
    self#sync_letter x y;

end

class clue_listbox ~parent ~label ~clues =
  object(self)
    initializer
      let scrx = Scrollbar.create ~orient:`Horizontal parent in
      let scry = Scrollbar.create ~orient:`Vertical parent in
      let label = Label.create ~text:label parent in
      let text = Listbox.create
          ~xscrollcommand:(Scrollbar.set scrx)
          ~yscrollcommand:(Scrollbar.set scry)
          ~background:(`Color "#FFFFFF")
          ~selectmode:`Browse
          parent
      in
      Scrollbar.configure ~command:(Listbox.yview text) scry;
      Scrollbar.configure ~command:(Listbox.xview text) scrx;
      Listbox.insert ~index:`End ~texts:clues text;
      pack [label] ~expand:false ~fill:`X ~side:`Top;
      pack [scrx] ~expand:false ~fill:`X ~side:`Bottom;
      pack [scry] ~expand:false ~fill:`Y ~side:`Right;
      pack [text] ~expand:true ~fill:`Both ~side:`Top
  end

class clue_widget ~parent ~clues =
  object(self)
    initializer
      let across_frame = Frame.create ~relief:`Groove ~borderwidth:1 parent in
      let down_frame = Frame.create ~relief:`Groove ~borderwidth:1 parent in
      let across = new clue_listbox ~parent:across_frame ~clues:clues.across
        ~label:"Across" in
      let down = new clue_listbox ~parent:down_frame ~clues:clues.down
        ~label:"Down" in
      pack [across_frame] ~side:`Top ~expand:true ~fill:`Both;
      pack [down_frame] ~side:`Bottom ~expand:true ~fill:`Both
  end

let _ =
  let input = { name = "lat140105.puz"; format = "acrosslite_binary" } in
  let xw = File.read input in
  let clue_frame = Frame.create ~relief:`Groove ~borderwidth:2 top in
  let xw_frame = Frame.create ~relief:`Groove ~borderwidth:2 top in
  let xw_widget = new xw_canvas ~parent:xw_frame ~xword:xw in
  let clue_widget = new clue_widget ~parent:clue_frame ~clues:xw.clues in
  pack [xw_frame] ~side:`Left ~expand:false ~anchor:`N;
  pack [clue_frame] ~side:`Right ~expand:true ~fill:`Both

let _ = Printexc.print mainLoop ();;
