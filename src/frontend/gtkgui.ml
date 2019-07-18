open StdLabels
open Xword.Types
open Xword.Utils
open Crosspad_model
open Model


let utf8 s = Glib.Convert.convert s ~from_codeset:"UTF-8"
   ~to_codeset:"ISO-8859-1"


module Colors = struct
  let black = (0.0, 0.0, 0.0)
  let white = (1.0, 1.0, 1.0)
  let light_green = (0.5, 1.0, 0.5)
  let dark_green = (0.0, 0.3, 0.0)
  let light_blue = (0.7, 0.7, 1.0)
end


let bg_of_cell model x y =
  let open Presenter in
  let open Colors in
  match cell_background x y model with
  | `Black -> black
  | `White -> white
  | `CursorBlack -> dark_green
  | `CursorWhite -> light_green
  | `CurrentWord -> light_blue
  | `CursorSymmBlack -> black
  | `CursorSymmWhite -> white


class grid_widget ~model ?packing ?show () =
  let sq_size = 30 in
  let id (x : Model.t) = x in
  let da = GMisc.drawing_area ?packing ?show () in
  object (self)
    inherit GObj.widget_full da#as_widget
    val model = model
    val mutable scale = sq_size

    initializer
      ignore @@ da#misc#connect#draw
        ~callback:(fun cr -> self#draw cr; true);
      ignore @@ da#event#connect#key_press
        ~callback:(fun ev -> self#handle_key_press ev);

      ignore @@ da#event#add [`BUTTON_PRESS];
      ignore @@ da#event#connect#button_press
        ~callback:(fun ev -> self#handle_button_press ev);

      ignore @@ da#misc#set_can_focus true;
      ignore @@ da#misc#grab_focus ();
      ()

    method handle_button_press ev =
      let point_to_cell x y =
        let f u = int_of_float u / scale in
        (f x, f y)
      in
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
        da#misc#queue_draw ();
        da#misc#grab_focus ()
      end;
      !handled

    method handle_key_press ev =
      let open GdkKeysyms in
      let state = GdkEvent.Key.state ev in
      let action = if List.mem `CONTROL ~set:state then
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
        da#misc#queue_draw ();
        true
      end

    method draw cr =
      let model = !model in
      let {Gtk.width=width; height=height; _} = da#misc#allocation in

      let div x y = int_of_float ((float_of_int x) /. (float_of_int y)) in
      let sx = div width model.xw.rows in
      let sy = div height model.xw.cols in
      scale <- min sx sy;

      let write_text ~row ~col ~font ~pos ~text =
        let layout = new GPango.layout (Cairo_pango.create_layout cr) in
        let desc = GPango.font_description_from_string font in
        layout#set_font_description desc;
        layout#set_text text;
        let top, left = row * scale, col * scale in
        let (w, h) = layout#get_size in
        let x0, y0 = float left, float top in
        let w = (float w /. float Pango.scale) /. 2. in
        let h = (float h /. float Pango.scale) in
        let s = float scale in
        let x, y = match pos with
          | `Letter -> (x0 +. s /. 2. -. w /. 2., y0 +. s -. h)
          | `Number -> (x0 +. 2., y0 +. 2.)
        in
        Cairo.save cr;
        Cairo.set_source_rgb cr 0. 0. 0.;
        Cairo.move_to cr x y;
        Cairo_pango.show_layout cr layout#as_layout;
        Cairo.restore cr
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

      let set_color color =
        let r, g, b = color in
        Cairo.set_source_rgb cr r g b
      in

      let rectangle ~x ~y ~w ~h ~color ~fill =
        let x, y, w, h = float x, float y, float w, float h in
        Cairo.save cr;
        set_color color;
        Cairo.rectangle cr x y ~w ~h;
        if fill then Cairo.fill cr else Cairo.stroke cr;
        Cairo.restore cr
      in

      set_color Colors.white;
      Cairo.paint cr;
      for y = 0 to model.xw.rows - 1 do
        for x = 0 to model.xw.cols - 1 do
          let top, left = y * scale, x * scale in

          (* cell *)
          let color = bg_of_cell model x y in
          rectangle ~x:left ~y:top ~w:scale ~h:scale ~color ~fill:true;
          Cairo.set_line_width cr 0.5;
          rectangle ~x:left ~y:top ~w:scale ~h:scale ~color:Colors.black ~fill:false;

          (* bars *)
          let sq = Xword.get model.xw x y in
          if sq.bar_right then begin
            let bx = left + scale - 2 in
            rectangle ~x:bx ~y:top ~w:2 ~h:scale ~color:Colors.black ~fill:true;
          end;
          if sq.bar_down then begin
            let by = top + scale - 2 in
            rectangle ~x:left ~y:by ~w:scale ~h:2 ~color:Colors.black ~fill:true;
          end;

          (* contents *)
          write_number ~row:y ~col:x;
          write_letter ~row:y ~col:x
        done
      done;

    method update =
      da#misc#queue_draw ()
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


let make_textview ~packing =
  let font_name = "Monospace 10" in
  let text = GText.view
      ~editable:false
      ~packing ~height:500 ~width:900
      ()
  in
  text#misc#modify_font_by_name font_name;
  text


class clues_text ~model ~packing () =
  let scrolled_win = GBin.scrolled_window
      ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC
      ~packing ()
  in
  let text = make_textview ~packing:scrolled_win#add in

  let format_clue (cl : working_clue) =
    Printf.sprintf "  %d. %s" cl.number cl.initial_clue
  in

  let format_clues () =
    let ac = "Across" :: "" :: CCList.map format_clue !model.clues_ac in
    let dn = "Down" :: "" :: CCList.map format_clue !model.clues_dn in
    CCString.unlines CCList.(ac @ [""] @ dn)
  in

  object(self)
    inherit GObj.widget_full scrolled_win#as_widget

    initializer
      self#update

    method update =
      let clues = format_clues () in
      text#buffer#set_text clues
  end


(* Metadata *)
class metadata_widget ~model ?packing ?show () =
  let scrolled_win =
    GBin.scrolled_window ?packing ?show
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()
  in
  let cols = new GTree.column_list in
  let key_col = cols#add Gobject.Data.string in
  let val_col = cols#add Gobject.Data.string in
  let list_model = GTree.list_store cols in
  let make_view ~column = make_cell_view ~column ~title:""
      ~opts: [ `XALIGN 0.; `YPAD 1 ]
  in
  let key_col_view = make_view ~column:key_col in
  let val_col_view = make_view ~column:val_col in
  let view = GTree.view ~model:list_model ~packing:scrolled_win#add () in
  object(self)
    inherit GObj.widget_full scrolled_win#as_widget

    initializer
      self#update;
      ignore @@ view#append_column key_col_view;
      ignore @@ view#append_column val_col_view;
      ()

    method update =
      List.iter ~f:(fun (k, v) ->
          let k = string_of_metadata_key k in
          let row = list_model#append () in
          list_model#set ~row ~column:key_col k;
          list_model#set ~row ~column:val_col v;
        )
        !model.xw.metadata
  end


let file_dialog ~title ~callback ~parent () =
  let dialog = GWindow.file_chooser_dialog
      ~action:`OPEN ~title ~parent () in
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `OPEN `OPEN ;
  begin match dialog#run () with
  | `OPEN -> begin
      match dialog#filename with
      | Some name -> callback name
      | _ -> ()
    end
  | `DELETE_EVENT | `CANCEL -> ()
  end ;
  dialog#destroy ()


class xword_widget ?packing ?show ~model ~window () =
  let hpane = GPack.paned `HORIZONTAL ?packing ?show () in
  let vpane = GPack.paned `VERTICAL ~packing:hpane#pack1 () in
  let fr = GBin.frame ~packing:vpane#pack1
      ~border_width:3 ~shadow_type:`IN () in
  let grid = new grid_widget ~packing:fr#add ~model () in
  let clues = new clues_text ~packing:hpane#pack2 ~model () in
  let meta = new metadata_widget ~packing:vpane#pack2 ~model () in
  object(self)
    inherit GObj.widget_full hpane#as_widget

    initializer
      vpane#set_position 1000;
      hpane#set_position 1000;
      self#update

    method load_file mode fname =
      let input = { name = fname; format = "across-lite-binary" } in
      let x = Converter.read_file input in
      let xw = match mode with
        | `Edit -> x
        | `Solve -> Xword.init_solve_mode x
      in
      model := {(Model.init xw) with file = input; edit_mode = mode};
      self#update

    method open_edit () =
      file_dialog ~parent:window ~title:"Open" ~callback:(self#load_file `Edit) ()

    method open_solve () =
      file_dialog ~parent:window ~title:"Open" ~callback:(self#load_file `Solve) ()

    method update =
      grid#update;
      clues#update;
      meta#update
  end


let make_combobox ~entries ~display ~f ~packing () =
  let cols = new GTree.column_list in
  let entry_col = cols#add Gobject.Data.caml in
  let label_col = cols#add Gobject.Data.string in
  let model = GTree.list_store cols in
  List.iter ~f:(fun e ->
      let row = model#append () in
      model#set ~row ~column:entry_col e;
      model#set ~row ~column:label_col (display e)
    ) entries;
  let combo = GEdit.combo_box ~model ~packing () in
  let renderer = GTree.cell_renderer_text [ `XPAD 5 ] in
  combo#pack renderer;
  combo#add_attribute renderer "text" label_col;
  ignore @@ combo#connect#changed
    ~callback:(fun () ->
       match combo#active_iter with
       | None -> ()
       | Some row -> 
         let data = combo#model#get ~row ~column:entry_col in
         f data);
  combo


class toolbar_widget ~model ?packing ?show () =
  let find_element e elts =
    let rec find e elts i = match elts with
      | [] -> 0
      | x::xs -> if (x = e) then i else find e xs (i + 1)
    in
    find e elts 0
  in
  let hbox = GPack.hbox ?packing ?show () in
  let packing = hbox#pack ~expand:false ~padding:3 in
  let _ = GMisc.label ~text:"Symmetry:" ~packing () in
  let symm_entries = [SymmNone; Symm90; Symm180] in
  let symm = make_combobox ~packing
      ~entries:symm_entries
      ~display:Presenter.display_symmetry
      ~f:(fun e -> model := {!model with symmetry = e}) ()
  in
  let _ = GMisc.separator `VERTICAL ~packing () in
  let _ = GMisc.label ~text:"Grid Editing:" ~packing () in
  let locked_entries = [true; false] in
  let locked = make_combobox ~packing
      ~entries:locked_entries 
      ~display:Presenter.display_locked
      ~f:(fun e -> model := {!model with grid_locked = e}) ()
  in
  object(self)
    inherit GObj.widget_full hbox#as_widget

    initializer
      self#update

    method update =
      symm#set_active (find_element !model.symmetry symm_entries);
      locked#set_active (find_element !model.grid_locked locked_entries)
  end

(* top level ui *)

let add_file_menu xword menubar =
  let open GdkKeysyms in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore @@ factory#add_item "Quit" ~key:_Q ~callback: GMain.quit;
  ignore @@ factory#add_item "Open (Edit mode)" ~key:_O ~callback:xword#open_edit;
  ignore @@ factory#add_item "Open (Solve mode)" ~key:_P ~callback:xword#open_solve

let make_ui model =
  let _locale = GMain.init ~setlocale:true () in
  let window = GWindow.window () in
  ignore @@ window#connect#destroy ~callback:GMain.quit;
  let vbox = GPack.vbox ~packing:window#add () in
  let menubar = GMenu.menu_bar ~packing:(vbox#pack ~expand:false) () in
  let _toolbar = new toolbar_widget
    ~packing:(vbox#pack ~expand:false) ~model () in
  let xword = new xword_widget ~packing:vbox#add ~model ~window () in
  let quit = GButton.button ~label:"Quit" ~packing:vbox#pack () in
  let _file_menu = add_file_menu xword menubar in
  ignore @@ quit#connect#clicked ~callback:GMain.quit;
  window

let () =
  let model = ref (Model.init (Xword.make 15 15)) in
  let window = make_ui model in
  window#show ();
  GMain.main ()
