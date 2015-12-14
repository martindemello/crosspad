open Core_kernel.Std

let read fname =
  let data = In_channel.read_all fname in
  Puz.read data
