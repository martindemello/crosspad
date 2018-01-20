open Puz_types
open Angstrom

module P = struct
  let is_digit = function 
    | '0' .. '9' -> true
    | _ -> false

  let is_alpha = function 
    | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false
end

let space = char ' '
let digit = satisfy P.is_digit
let digits = take_while1 P.is_digit
let nul = char '\000'
let alphas = take_while1 P.is_alpha
let eof = end_of_input

let int_pair x y = (int_of_string x, int_of_string y)

(* LTIM *)
let ltim = lift2 int_pair
    (digits <* (char ','))
    (digits <* nul)

(* RTBL *)
let n_digit n = count n digit >>| CCString.of_list

let rtbl_num = (space *> n_digit 1) <|> (n_digit 2)

let rtbl_entry = lift2 (fun x y -> (int_of_string x, y))
    (rtbl_num <* (char ':'))
    (alphas <* (char ';'))

let rtbl = (many1 rtbl_entry) <* nul

(* TEXT SECTION *)

let text_section_header =
  (char '<') *> alphas <* (char '>') <* eof

let text_size = lift2 int_pair
    (digits <* (char 'x'))
    (digits <* eof)

let run_match p s =
  match parse_string p s with
  | Result.Ok x -> Some x
  | _ -> None

(*$T match_ltim
  match_ltim "1,2\000" = Some (1, 2)
  match_ltim "10,22\000" = Some (10, 22)
  match_ltim "10,22" = None
  match_ltim "1022" = None
  match_ltim "1022\000" = None
*)
let match_ltim = run_match ltim

(*$T match_rtbl
  match_rtbl " 1:foo; 2:bar;\000" = Some [1, "foo"; 2, "bar"]
  match_rtbl " 1:foo;12:bar;\000" = Some [1, "foo"; 12, "bar"]
  match_rtbl " 1:foo;123:bar;\000" = None
  match_rtbl " 1:foo; 2:bar;" = None
  match_rtbl "1:foo;3:bar;\000" = None
*)
let match_rtbl = run_match rtbl

(*$T match_text_section
  match_text_section "<HELLO>" = Some "HELLO"
  match_text_section "<HELLO>world" = None
  match_text_section "<HELLO" = None
  match_text_section "HELLO>" = None
  match_text_section "HELLO" = None
*)
let match_text_section = run_match text_section_header

(*$T match_text_size
  match_text_size "10x12" = Some (10, 12)
  match_text_size "10xfoo" = None
  match_text_size "foox12" = None
  match_text_size "10x12.1" = None
*)
let match_text_size = run_match text_size
