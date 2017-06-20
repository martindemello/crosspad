open StdLabels
open Xword.Types
open Cursor
open Utils
open Crosspad_model
open Model

[@@@ ocaml.warning "-10"]

let utf8 s = Glib.Convert.convert s "UTF-8" "ISO-8859-1"

let check_cache ~cond ~create ~destroy = function
    Some pm ->
      if cond pm then pm else begin
        destroy pm;
        create ()
      end
  | None -> create ()

let bg_of_cell model x y =
  let open Presenter in
  match cell_background x y model with
  | `Black -> `BLACK
  | `White -> `WHITE
  | `CursorBlack -> `NAME "dark green"
  | `CursorWhite -> `NAME "light green"
  | `CurrentWord -> `NAME "light blue"
  | `CursorSymmBlack -> `BLACK
  | `CursorSymmWhite -> `WHITE

class xw_widget ~model ?packing ?show () =
  let scale = 30 in
  let id (x : Model.t) = x in
  let width = !model.xw.cols * scale + 1 in
  let height = !model.xw.rows * scale + 1 in
  let da = GMisc.drawing_area ~width ~height ?packing ?show () in
  let context = da#misc#create_pango_context in
  let point_to_cell x y =
    let f u = int_of_float u / scale in
    (f x, f y)
  in
  object (self)
    inherit GObj.widget_full da#as_widget
    val model = model
    val mutable size = 0, 0
    val mutable pixmap = None

    initializer
      ignore @@ da#event#connect#expose ~callback:(fun _ -> self#draw; true);
      ignore @@ da#event#connect#key_press
        ~callback:(fun ev -> self#handle_key_press ev);

      da#event#add [`BUTTON_PRESS];
      ignore @@ da#event#connect#button_press
        ~callback:(fun ev -> self#handle_button_press ev);

      da#misc#set_can_focus true;
      da#misc#grab_focus ();
      ()

    method handle_button_press ev =
      let handled = ref true in
      let _ = match GdkEvent.Button.button ev with
      | 1 -> (
          let x = GdkEvent.Button.x ev in
          let y = GdkEvent.Button.y ev in
          let x, y = point_to_cell x y in
          let action = Action.SetCursor (x, y) in
          model := Controller.update action (!model, id);
        )
      | _ -> handled := false
      in
      if !handled then begin
        self#draw;
        da#misc#grab_focus ()
      end;
      !handled

    method handle_key_press ev =
      let open GdkKeysyms in
      let state = GdkEvent.Key.state ev in
      let action = if List.mem `CONTROL state then
         match GdkEvent.Key.keyval ev with
          | k when k = _Left -> Action.ToggleBar `Left
          | k when k = _KP_Left -> Action.ToggleBar `Left
          | k when k = _Right -> Action.ToggleBar `Right
          | k when k = _KP_Right -> Action.ToggleBar `Right
          | k when k = _Up -> Action.ToggleBar `Up
          | k when k = _KP_Up -> Action.ToggleBar `Up
          | k when k = _Down -> Action.ToggleBar `Down
          | k when k = _KP_Down -> Action.ToggleBar `Down
          | _ -> Action.Nothing
      else
        match GdkEvent.Key.keyval ev with
        | k when k = _Left -> Action.MoveCursor `Left
        | k when k = _KP_Left -> Action.MoveCursor `Left
        | k when k = _Right -> Action.MoveCursor `Right
        | k when k = _KP_Right -> Action.MoveCursor `Right
        | k when k = _Up -> Action.MoveCursor `Up
        | k when k = _KP_Up -> Action.MoveCursor `Up
        | k when k = _Down -> Action.MoveCursor `Down
        | k when k = _KP_Down -> Action.MoveCursor `Down
        | k when k = _space -> Action.ToggleBlack
        | k when k = _Page_Up -> Action.ToggleDir
        | k when k = _Delete -> Action.Delete
        | k when k = _BackSpace -> Action.Backspace
        | k -> begin
           match Presenter.letter_of_code k with
             | Some s -> Action.SetLetter s
             | _ -> Action.Nothing
          end
      in
      model := Controller.update action (!model, id);
      if action == Action.Nothing then
        false
      else begin
        self#draw;
        true
      end

    method draw =
      let model = !model in
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
          | `Letter -> (left + scale / 2 - w / 2, top + scale - h)
          | `Number -> (left + 2, top + 2)
        in
        dr#put_layout ~x ~y ~fore:`BLACK layout;
      in

      let write_letter ~row ~col =
        let cell = Xword.get_cell model.xw col row in
        match (Presenter.letter_of_cell cell) with
        | "" -> ()
        | c -> write_text ~row ~col ~text:c ~pos:`Letter ~font:"sans 12"
      in

      let write_number ~row ~col =
        let num = Xword.get_num model.xw col row in
        match (Presenter.display_num num) with
        | "" -> ()
        | c -> write_text ~row ~col ~text:c ~pos:`Number ~font:"sans 6"
      in

      dr#set_foreground `WHITE;
      dr#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
      for y = 0 to model.xw.rows - 1 do
        for x = 0 to model.xw.cols - 1 do
          let top, left = y * scale, x * scale in
          let rect = dr#rectangle ~x:left ~y:top ~width:scale ~height:scale in
          (* cell *)
          dr#set_foreground (bg_of_cell model x y);
          rect ~filled:true ();
          dr#set_foreground `BLACK;
          rect ~filled:false ();

          (* bars *)
          let sq = Xword.get model.xw x y in
          if sq.bar_right then begin
            let bx = left + scale - 2 in
            dr#rectangle ~x:bx ~y:top ~width:2 ~height:scale ~filled:true ()
          end;
          if sq.bar_down then begin
            let by = top + scale - 2 in
            dr#rectangle ~x:left ~y:by ~width:scale ~height:2 ~filled:true ()
          end;

          (* contents *)
          write_number ~row:y ~col:x;
          write_letter ~row:y ~col:x
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

class clue_widget ~model ~dir ?packing ?show () =
  let scrolled_win =
    GBin.scrolled_window ?packing ~width:200
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()
  in
  let cols = new GTree.column_list in
  let column = cols#add Gobject.Data.string in
  let list_model = GTree.list_store cols in
  let clues = Xword.get_clues !model.xw dir in
  let title = cluebox_title dir in
  let clue_col_view = make_cell_view ~column ~title
      ~opts: [ `XALIGN 0.; `YPAD 1 ]
  in
  let view = GTree.view ~model:list_model ~packing:scrolled_win#add () in
  object(self)
    inherit GObj.widget_full scrolled_win#as_widget

    initializer
      List.iter ~f:(fun clue ->
          let row = list_model#append () in
          list_model#set ~row ~column clue)
        (List.map Presenter.format_clue clues);
      ignore @@ view#append_column clue_col_view
  end

class clues_widget ~model ?packing ?show () =
  let vbox = GPack.vbox ?packing ?show () in
  let _ac = new clue_widget ~model ~dir:`Across ~packing:vbox#add ?show () in
  let _dn = new clue_widget ~model ~dir:`Down ~packing:vbox#add ?show () in
  object(self)
    inherit GObj.widget_full vbox#as_widget
  end

(* Metadata *)
class metadata_widget ~model ?packing ?show () =
  let scrolled_win =
    GBin.scrolled_window ?packing
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()
  in
  let cols = new GTree.column_list in
  let key_col = cols#add Gobject.Data.string in
  let val_col = cols#add Gobject.Data.string in
  let list_model = GTree.list_store cols in
  let make_view ~column = make_cell_view ~column ~title:""
      ~opts: [ `XALIGN 0.; `YPAD 1 ]
  in
  let key_col_view = make_view key_col in
  let val_col_view = make_view val_col in
  let view = GTree.view ~model:list_model ~packing:scrolled_win#add () in
  object(self)
    inherit GObj.widget_full scrolled_win#as_widget

    initializer
      List.iter ~f:(fun (k, v) ->
          let k = string_of_metadata_key k in
          let row = list_model#append () in
          list_model#set ~row ~column:key_col k;
          list_model#set ~row ~column:val_col v;
        )
        !model.xw.metadata;
      ignore @@ view#append_column key_col_view;
      ignore @@ view#append_column val_col_view
  end

let add_file_menu menubar =
  let open GdkKeysyms in

  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore @@ factory#add_item "Quit" ~key:_Q ~callback: GMain.quit

let () =
  let _locale = GMain.init ~setlocale:true () in
  let w = GWindow.window () in
  ignore @@ w#connect#destroy ~callback:GMain.quit;
  let vbox = GPack.vbox ~packing:w#add () in
  let menubar = GMenu.menu_bar ~packing:(vbox#pack ~expand:false) () in
  let _ = add_file_menu menubar in

  let hbox = GPack.hbox ~packing:vbox#add () in
  let vb1 = GPack.vbox ~packing:(hbox#pack ~expand:false) () in
  let fr = GBin.frame ~border_width:3 ~shadow_type:`IN
      ~packing:(vb1#pack ~expand:false) () in
  let solve_mode = true in
  let xw =
    if Array.length Sys.argv > 1 then
      let fname = Sys.argv.(1) in
      let input = { name = fname; format = "across-lite-binary" } in
      let x = Converter.read_file input in
      if solve_mode then Xword.clear_fill x else x
    else
      Xword.make 15 15 |> Xword.renumber
  in
  let model = ref (Model.init xw) in
  let _xword = new xw_widget ~packing:fr#add ~model () in
  let _clues = new clues_widget ~packing:hbox#add ~model () in
  let _meta = new metadata_widget ~packing:vb1#add ~model () in
  let quit = GButton.button ~label:"Quit" ~packing:vbox#pack () in
  ignore @@ quit#connect#clicked ~callback:GMain.quit;
  w#show ();
  GMain.main ()
