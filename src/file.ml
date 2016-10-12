open Types
open Converter

let read file =
  let data = CCIO.(with_in file.name read_all) in
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
  CCIO.(with_out file.name (fun oc -> write_line oc data))
