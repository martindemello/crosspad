open Types
open Utils

let readers = [
  "across-lite-binary", (module Puz : READER);
  "json", (module Json : READER)
]

let writers = [
  "across-lite-binary", (module Puz : WRITER);
  "across-lite-text", (module Ac_text : WRITER);
  "json", (module Json : WRITER)
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
  CCResult.map apply (get_reader format)

let write format xw =
  let apply w =
    let (module W : WRITER) = w in
    W.write xw
  in
  CCResult.map apply (get_writer format)

let convert input =
  let open CCResult.Infix in
  read input.input_format input.data >>= (fun xw ->
      write input.output_format xw)
