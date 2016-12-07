open StdLabels
open Typedefs
open Cursor
open Utils

[@@@ ocaml.warning "-10"]

let utf8 s = Glib.Convert.convert s "UTF-8" "ISO-8859-1"

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
  let width = xw.cols * scale + 1 in
  let height = xw.rows * scale + 1 in
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
    val mutable dir : word_direction = `Across
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

    method delete_letter ~bksp =
      if bksp then begin
        let d = match dir with `Across -> `Bksp_Ac | `Down -> `Bksp_Dn in
        self#move_cursor ~wrap:false d
      end;
      if Xword.delete_letter xw cursor.x cursor.y then begin
        Xword.renumber xw |> ignore;
      end;
      self#draw

    method set_letter c =
      let s = Char.uppercase_ascii c |> String.make 1 in
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
      | k when k = GdkKeysyms._Delete -> self#delete_letter ~bksp:false
      | k when k = GdkKeysyms._BackSpace -> self#delete_letter ~bksp:true
      | k when is_letter k -> self#set_letter (Char.chr k)
      | _ -> handled := false
      in
      !handled

    method draw =
      let {Gtk.x=x0; y=y0; width=width; height=height} =
        da#misc#allocation in
      let _size = (min width height) * 49 / 50 in
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

let make_cell_view ~column ~title ~opts =
  let renderer = GTree.cell_renderer_text opts in
  let col = GTree.view_column ~title () in
  col#pack renderer;
  col#set_cell_data_func renderer
    (fun model row ->
       let str = model#get ~row ~column in
       renderer#set_properties [ `TEXT (utf8 str) ]);
  col

(* Clues *)
let cluebox_title (dir : word_direction) = match dir with
  | `Across -> "Across"
  | `Down -> "Down"

class clue_widget ~xw ~dir ?packing ?show () =
  let scrolled_win =
    GBin.scrolled_window ?packing ~width:200
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()
  in
  let cols = new GTree.column_list in
  let column = cols#add Gobject.Data.string in
  let model = GTree.list_store cols in
  let clues = Xword.get_clues xw dir in
  let title = cluebox_title dir in
  let clue_col_view = make_cell_view ~column ~title
      ~opts: [ `XALIGN 0.; `YPAD 1 ]
  in
  let view = GTree.view ~model ~packing:scrolled_win#add () in
  object(self)
    inherit GObj.widget_full scrolled_win#as_widget

    initializer
      List.iter ~f:(fun clue ->
          let row = model#append () in
          model#set ~row ~column clue)
        clues;
      view#append_column clue_col_view; ()
  end

class clues_widget ~xw ?packing ?show () =
  let vbox = GPack.vbox ?packing ?show () in
  let _ac = new clue_widget ~xw ~dir:`Across ~packing:vbox#add ?show () in
  let _dn = new clue_widget ~xw ~dir:`Down ~packing:vbox#add ?show () in
  object(self)
    inherit GObj.widget_full vbox#as_widget
  end

(* Metadata *)
class metadata_widget ~xw ?packing ?show () =
  let scrolled_win =
    GBin.scrolled_window ?packing
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()
  in
  let cols = new GTree.column_list in
  let key_col = cols#add Gobject.Data.string in
  let val_col = cols#add Gobject.Data.string in
  let model = GTree.list_store cols in
  let make_view ~column = make_cell_view ~column ~title:""
      ~opts: [ `XALIGN 0.; `YPAD 1 ]
  in
  let key_col_view = make_view key_col in
  let val_col_view = make_view val_col in
  let view = GTree.view ~model ~packing:scrolled_win#add () in
  object(self)
    inherit GObj.widget_full scrolled_win#as_widget

    initializer
      List.iter ~f:(fun (k, v) ->
          let k = string_of_metadata_key k in
          let row = model#append () in
          model#set ~row ~column:key_col k;
          model#set ~row ~column:val_col v;
        )
        xw.metadata;
      view#append_column key_col_view;
      view#append_column val_col_view; ()
  end


let () =
  let _locale = GMain.init ~setlocale:true () in
  let w = GWindow.window () in
  w#connect#destroy ~callback:GMain.quit;
  let vbox = GPack.vbox ~packing:w#add () in
  let hbox = GPack.hbox ~packing:vbox#add () in
  let vb1 = GPack.vbox ~packing:(hbox#pack ~expand:false) () in
  let fr = GBin.frame ~border_width:3 ~shadow_type:`IN
      ~packing:(vb1#pack ~expand:false) () in
  let fname = Sys.argv.(1) in
  let input = { name = fname; format = "acrosslite_binary" } in
  let xw = File.read input in
  let _xword = new xw_widget ~packing:fr#add ~xw:xw () in
  let _clues = new clues_widget ~packing:hbox#add ~xw () in
  let _meta = new metadata_widget ~packing:vb1#add ~xw () in
  let quit = GButton.button ~label:"Quit" ~packing:vbox#pack () in
  quit#connect#clicked ~callback:GMain.quit;
  w#show ();
  GMain.main ()
