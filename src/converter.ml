open Types
open Utils

let readers = [
  "acrosslite_binary", (module Puz : READER);
  "json", (module Json : READER)
]

let writers = [
  "acrosslite_binary", (module Puz : WRITER);
  "acrosslite_text", (module Ac_text : WRITER);
  "json", (module Json : WRITER)
]

let get_reader format = assoc_in_list readers format

let get_writer format = assoc_in_list writers format

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


