open Types
open Core_kernel.Std

let readers = [
  "acrosslite_binary", (module Puz : READER)
]

let writers = [
  "acrosslite_binary", (module Puz : WRITER);
  "acrosslite_text", (module Ac_text : WRITER)
]

let get_reader format = List.Assoc.find readers format

let get_writer format = List.Assoc.find writers format

let read file =
  let data = In_channel.read_all file.name in
  let reader = get_reader file.format in
  match reader with
  | Some reader -> let (module R) = reader in R.read data
  | None -> Xword.make 15 15 (* TODO: Handle the error *)

let write file xw =
  let writer = get_writer file.format in
  let data = match writer with
  | Some writer -> let (module W) = writer in W.write xw
  | None -> "" (* TODO: Handle the error *)
  in
  Out_channel.write_all file.name data

let convert input =
  let reader = get_reader input.input_format in
  let writer = get_writer input.input_format in
  match (reader, writer) with
  | Some r, Some w -> begin
      let (module R) = r in
      let (module W) = w in
      let xw = R.read input.data in
      let out = W.write xw in
      { output = out; error = "" }
    end
  | None, _ -> { output = "";
                 error = "No reader available for " ^ input.input_format }
  
  | _, None -> { output = "";
                 error = "No writer available for " ^ input.output_format }

