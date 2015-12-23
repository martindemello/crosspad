open Types
open Core_kernel.Std

let () =
  let input = { name = "lat140105.puz"; format = "acrosslite_binary" } in
  let output = { name = "test.puz"; format = "acrosslite_binary" } in
  let xw = File.read input in
  File.write output xw
