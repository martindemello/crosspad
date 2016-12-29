open Opium.Std

open Xword.Types
open Printf

type key = string
type value = string list
type params = (key * value) list

let to_s = function None -> print_string "none "
                  | Some x -> printf "%s " x

let to_bs = function None -> print_string "none "
                  | Some x -> print_string "some "

let display k f =
  Printf.printf "%s: " k;
  List.iter to_s f;
  print_newline ();
  f

let display_list k f =
  Printf.printf "%s: " k;
  List.iter to_bs f;
  print_newline ();
  f

let headers = Cohttp.Header.init_with "Access-Control-Allow-Origin" "*"

(* Get exactly one value per key - return None for
 * missing values, take the first value otherwise *)
let get_params pairs keys =
  let get k = CCList.Assoc.get pairs k in
  print_endline "\ngetting params\n";
  List.map get keys |> display_list "keys"
  |> List.map (CCOpt.flat_map CCList.head_opt) |> display "vals"
  |> CCOpt.sequence_l

let handle_post req keys f =
  let open Lwt in
  req |> App.string_of_body_exn >|= (fun s -> respond ~headers (`String s))

  (*
  req |> App.urlencoded_pairs_of_body >|= (fun params ->
      let xs = get_params params keys in
      Printf.printf "%d\n" @@ List.length params;
      List.iter (fun (a, b) -> print_string a) params;
      match xs with
      | Some c -> f c
      | None -> respond ~headers (`Json (Json.error "Missing parameters"))
    )
     *)

[@@@ ocaml.warning "-8"]

(* filedata -> json *)
let to_json req =
  handle_post req ["filedata"; "from"] begin fun params ->
    let [data; input_format] = params in
    let conv = { data; input_format; output_format = "json" } in
    match Converter.to_json conv with
    | Ok json -> respond ~headers (`Json json)
    | Error err -> respond ~headers (`Json (Json.error err)) 
  end

(* json -> application/octet-stream *)
let to_blob req =
  handle_post req ["filedata"; "to"] begin fun params ->
    let [data; output_format] = params in
    let conv = { data; output_format; input_format = "json" } in
    match Converter.to_json conv with
    | Ok json -> respond (`Json json)
    | Error err -> respond (`Json (Json.error err)) 
  end

let _ =
  App.empty
  |> post "/to_json" to_json
  |> post "/to_blob" to_blob
  |> App.run_command
