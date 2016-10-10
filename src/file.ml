open Types
open Converter
open Core_kernel.Std

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
