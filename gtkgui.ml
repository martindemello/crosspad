open StdLabels
open Types

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
  object (self)
    inherit GObj.widget_full da#as_widget
    val rows = xw.rows
    val cols = xw.cols
    val mutable cursor = Cursor.make xw.rows xw.cols
    val mutable size = 0, 0
    val mutable pixmap = None
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

    initializer
      da#event#connect#expose ~callback:(fun _ -> self#draw; true); ()
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
