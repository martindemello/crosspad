open Xword.Types
open Utils

module Json = Xw_json

let readers = [
  "across-lite-binary", (module Puz : READER);
  "across-lite-text", (module Ac_text : READER);
  "json", (module Json : READER);
  "text", (module Text : READER)
]

let writers = [
  "across-lite-binary", (module Puz : WRITER);
  "across-lite-text", (module Ac_text : WRITER);
  "json", (module Json : WRITER);
  "reddit-filled", (module Reddit.Filled : WRITER);
  "reddit-blank", (module Reddit.Blank : WRITER);
  "text", (module Text : WRITER)
]

let keys xs = List.map fst xs |> List.sort compare

let reader_list = keys readers

let writer_list = keys writers

let get_reader format =
  match assoc_in_list readers format with
   | Some r -> Ok r
   | None -> Error ("No reader for " ^ format)

let get_writer format =
  match assoc_in_list writers format with
   | Some w -> Ok w
   | None -> Error ("No writer for " ^ format)

let read format data =
  let apply r =
    let (module R : READER) = r in
    R.read data
  in
  try
    CCResult.map apply (get_reader format)
  with
  | PuzzleFormatError e -> Error e

let write format xw =
  let apply w =
    let (module W : WRITER) = w in
    W.write xw
  in
  try
    CCResult.map apply (get_writer format)
  with
  | PuzzleFormatError e -> Error e

let convert input =
  let open CCResult.Infix in
  try
    read input.input_format input.data
        >>= write input.output_format
  with
  | PuzzleFormatError e -> Error e

let to_json input =
  let open CCResult.Infix in
  try
    read input.input_format input.data
    >|= Json.write_json
  with
  | PuzzleFormatError e -> Ok (Json.error e)

(*******************************************************
 * File I/O
 *******************************************************)

let read_file file =
  let data = CCIO.(with_in file.name read_all) in
  let res = read file.format data in
  match res with
  | Ok xw -> xw
  | Error e -> (
      Printf.printf "ERROR: %s\n" e;
      Xword.make 15 15 (* TODO: Handle the error *)
    )

let write_file file xw =
  let res = write file.format xw in
  let data = match res with
  | Ok data -> data
  | Error e -> (
      Printf.printf "ERROR: %s\n" e;
      "" (* TODO: Handle the error *)
    )
  in
  CCIO.(with_out file.name (fun oc -> write_line oc data))
