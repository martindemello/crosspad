open Types

let () =
  let input = { name = "test.txt"; format = "json" } in
  let output = { name = "test.ac.txt"; format = "acrosslite_text" } in
  let xw = File.read input in
  File.write output xw
