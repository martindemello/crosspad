open Types
open Core_kernel.Std

let readers = [
  "acrosslite_binary", (module Puz : READER)
]

let get_reader format = List.Assoc.find readers format

let read file =
  let data = In_channel.read_all file.name in
  let reader = get_reader file.format in
  match reader with
  | Some reader -> let (module R) = reader in R.read data
  | None -> Xword.make 15 15 (* TODO: Handle the error *)
