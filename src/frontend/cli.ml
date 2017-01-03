open Cmdliner
open Printf

open Xword.Types

let convert in_format out_format in_file out_file =
  let input = { name = in_file; format = in_format } in
  let output = { name = out_file; format = out_format } in
  let xw = Converter.read_file input in
  Converter.write_file output xw

let display_format_list () =
  let show xs =
    List.iter (printf "- %s\n") xs
  in
  print_endline "Convert from:";
  print_endline "-----------------------";
  show Converter.reader_list;
  print_newline ();
  print_endline "Convert to:";
  print_endline "-----------------------";
  show Converter.writer_list

(* Command line interface *)

let usage = {|
Usage:
  xwconvert -f <from> -t <to> -i <input> -o <output>

Options:
-f, --from=<s>    Format to convert from
-t, --to=<s>      Format to convert to
-i, --in=<s>      Input file
-o, --out=<s>     Output file
-l, --list        List available format plugins
-v, --version     Print version and exit
-h, --help        Show this message
|}


let handle list_formats in_format out_format in_file out_file =
  if list_formats then
    `Ok (display_format_list ())
  else
    match in_format, out_format, in_file, out_file with
    | Some f, Some t, Some i, Some o -> `Ok (convert f t i o)
    | _ -> `Error (false, usage)

let in_file =
  let doc = "Input filename" in
  Arg.(value & opt (some string) None & info ["i"; "input"] ~doc)

let out_file =
  let doc = "Output filename" in
  Arg.(value & opt (some string) None & info ["o"; "output"] ~doc)

let in_format =
  let doc = "Format to convert from" in
  Arg.(value & opt (some string) None & info ["f"; "from"] ~doc)

let out_format =
  let doc = "Format to convert to" in
  Arg.(value & opt (some string) None & info ["t"; "to"] ~doc)

let list_formats =
  let doc = "List available formats" in
  Arg.(value & flag & info ["l"; "list"] ~doc)

let cmd =
  let doc = "Crossword file format converter" in
  Term.(ret (const handle $ list_formats $ in_format $ out_format $ in_file $ out_file)),
  Term.info "xwconvert" ~version:"0.1" ~doc

let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
