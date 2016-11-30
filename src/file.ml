open Types
open Converter

let read file =
  let data = CCIO.(with_in file.name read_all) in
  let res = read file.format data in
  match res with
  | Ok xw -> xw
  | _ -> Xword.make 15 15 (* TODO: Handle the error *)

let write file xw =
  let res = write file.format xw in
  let data = match res with
  | Ok data -> data
  | _ -> "" (* TODO: Handle the error *)
  in
  CCIO.(with_out file.name (fun oc -> write_line oc data))
