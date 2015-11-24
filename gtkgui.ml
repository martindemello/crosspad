open StdLabels
open Types
open Cursor

let check_cache ~cond ~create ~destroy = function
    Some pm ->
      if cond pm then pm else begin
        destroy pm;
        create ()
      end
  | None -> create ()

let letter_of_cell = function
  | Letter c -> c
  | Rebus r -> r.display_char
  | _ -> ""

let bg_of_cell cell is_cursor =
  match cell, is_cursor with
  | Black, false -> `BLACK
  | Black, true -> `NAME "dark green"
  | _, false -> `WHITE
  | _, true -> `NAME "light green"

class xw_widget ~xw ?packing ?show () =
  let scale = 30 in
  let width = xw.cols * scale in
  let height = xw.rows * scale in
  let da = GMisc.drawing_area ~width ~height ?packing ?show () in
  let context = da#misc#create_pango_context in
  let point_to_cell x y =
    let f u = int_of_float u / scale in
    (f x, f y)
  in
  object (self)
    inherit GObj.widget_full da#as_widget
    val rows = xw.rows
    val cols = xw.cols
    val mutable cursor = Cursor.make xw.rows xw.cols
    val mutable dir : [`Across | `Down] = `Across
    val mutable size = 0, 0
    val mutable pixmap = None

    initializer
      da#event#connect#expose ~callback:(fun _ -> self#draw; true);
      da#event#connect#key_press
        ~callback:(fun ev -> self#handle_key_press ev);

      da#event#add [`BUTTON_PRESS];
      da#event#connect#button_press
        ~callback:(fun ev -> self#handle_button_press ev);

      da#misc#set_can_focus true;
      da#misc#grab_focus ();
      ()

    method toggle_dir =
      dir <- match dir with `Across -> `Down | `Down -> `Across

    method toggle_black =
      if Xword.toggle_black xw cursor.x cursor.y then begin
        Xword.renumber xw |> ignore;
        self#draw
      end

    method set_letter c =
      let s = Char.uppercase c |> String.make 1 in
      Xword.set_cell xw cursor.x cursor.y (Letter s);
      self#move_cursor ~wrap:false (dir :> direction)

    method move_cursor ?wrap:(wrap=true) (d : direction) =
      let new_cursor = Cursor.move cursor ~wrap:wrap d in
      self#set_cursor new_cursor

    method set_cursor new_cursor =
      cursor <- new_cursor;
      self#draw

    method handle_button_press ev =
      let handled = ref true in
      let _ = match GdkEvent.Button.button ev with
      | 1 -> (
          let x = GdkEvent.Button.x ev in
          let y = GdkEvent.Button.y ev in
          let x, y = point_to_cell x y in
          self#set_cursor {cursor with x; y}
        )
      | _ -> handled := false
      in
      !handled

    method handle_key_press ev =
      let is_letter k = (Char.code 'a') <= k && k <= (Char.code 'z') in
      let handled = ref true in
      let _ = match GdkEvent.Key.keyval ev with
      | k when k = GdkKeysyms._Left -> self#move_cursor `Left
      | k when k = GdkKeysyms._KP_Left -> self#move_cursor `Left
      | k when k = GdkKeysyms._Right -> self#move_cursor `Right
      | k when k = GdkKeysyms._KP_Right -> self#move_cursor `Right
      | k when k = GdkKeysyms._Up -> self#move_cursor `Up
      | k when k = GdkKeysyms._KP_Up -> self#move_cursor `Up
      | k when k = GdkKeysyms._Down -> self#move_cursor `Down
      | k when k = GdkKeysyms._KP_Down -> self#move_cursor `Down
      | k when k = GdkKeysyms._space -> self#toggle_black
      | k when k = GdkKeysyms._Page_Up -> self#toggle_dir
      | k when is_letter k -> self#set_letter (Char.chr k)
      | _ -> handled := false
      in
      !handled

    method draw =
      let {Gtk.x=x0; y=y0; width=width; height=height} =
        da#misc#allocation in
      let size = (min width height) * 49 / 50 in
      let dr = check_cache pixmap
          ~cond:(fun pm -> pm#size = (width, height))
          ~destroy:(fun pm -> Gdk.Pixmap.destroy pm#pixmap)
          ~create: (fun () -> GDraw.pixmap ~width ~height ~window:da ())
      in
      pixmap <- Some dr;

      let write_text ~row ~col ~font ~pos ~text =
        context#set_font_by_name font;
        let layout = context#create_layout in
        Pango.Layout.set_text layout text;
        let (w,h) = Pango.Layout.get_pixel_size layout in
        let top, left = row * scale, col * scale in
        let x, y = match pos with
          | `Letter -> (left + 4 , top + scale - h)
          | `Number -> (left + scale - w, top + 1)
        in
        dr#put_layout ~x ~y ~fore:`BLACK layout;
      in

      let write_letter ~row ~col ~cell =
        match (letter_of_cell cell) with
        | "" -> ()
        | c -> write_text ~row ~col ~text:c ~pos:`Letter ~font:"sans 12"
      in

      let write_number ~row ~col =
        let num = Xword.get_num xw col row in
        if num != 0 then begin
          let text = string_of_int num in
          write_text ~row ~col ~text ~pos:`Number ~font:"sans 6"
        end
      in

      dr#set_foreground `WHITE;
      dr#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
      for y = 0 to rows - 1 do
        for x = 0 to cols - 1 do
          let cell = Xword.get_cell xw x y in
          let is_cursor = (x, y) = (cursor.x, cursor.y) in
          let top, left = y * scale, x * scale in
          let rect = dr#rectangle ~x:left ~y:top ~width:scale ~height:scale in
          (* cell *)
          dr#set_foreground (bg_of_cell cell is_cursor);
          rect ~filled:true ();
          dr#set_foreground `BLACK;
          rect ~filled:false ();
          (* contents *)
          write_number ~row:y ~col:x;
          write_letter ~row:y ~col:x ~cell
        done
      done;

      (new GDraw.drawable da#misc#window)#put_pixmap ~x:0 ~y:0 dr#pixmap
  end

let () =
  let w = GWindow.window () in
  w#connect#destroy ~callback:GMain.quit;
  let hbox = GPack.hbox ~packing:w#add () in
  let fr = GBin.frame ~border_width:3 ~shadow_type:`IN ~packing:hbox#add () in
  let xw = File.read "lat140105.puz" in
  let xword = new xw_widget ~packing:fr#add ~xw:xw () in
  let vbox = GPack.vbox ~border_width:3 ~spacing:4 ~packing:hbox#pack () in
  let quit = GButton.button ~label:"Quit" ~packing:vbox#pack () in
  quit#connect#clicked ~callback:GMain.quit;
  w#show ();
  GMain.main ()
