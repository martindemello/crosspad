open Angstrom
open Typedefs

let run_match p s =
  match parse_string p s with
  | Result.Ok x -> Some x
  | _ -> None

module P = struct
  let is_digit = function 
    | '0' .. '9' -> true
    | _ -> false

  let is_alpha = function 
    | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false

  let is_eol =
    function | '\r' | '\n' -> true
    | _ -> false
end

let space = char ' '
let digit = satisfy P.is_digit
let digits = take_while1 P.is_digit
let nul = char '\000'
let alphas = take_while1 P.is_alpha
let not_alphas = take_till P.is_alpha
let eof = end_of_input
let eol = end_of_line

let rest_of_line = take_till P.is_eol

let blank_line = (many space) <* eol
