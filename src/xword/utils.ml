open Typedefs

let string_of_metadata_key = function
| `Copyright -> "Copyright"
| `Publisher -> "Publisher"
| `Publication -> "Publication"
| `Url -> "Url"
| `Uniqueid -> "Uniqueid"
| `Title -> "Title"
| `Intro -> "Intro"
| `Explanation -> "Explanation"
| `Annotation -> "Annotation"
| `Author -> "Author"
| `Editor -> "Editor"
| `Date -> "Date"
| `Notes -> "Notes"
| `Difficulty -> "Difficulty"
| `Origin -> "Origin"
| `Block -> "Block"
| `Empty -> "Empty"
| `Styles -> "Styles"
| _ -> "UNKNOWN"

(* map find returning option *)
let smap_find key map =
  try Some (SMap.find key map) with
  | Not_found -> None

let list_assoc key xs =
  try Some (List.assoc key xs) with
  | Not_found -> None

let assoc_in_list xs key =
  list_assoc key xs

let split_lines x =
  Str.split (Str.regexp "\n") x

let unlines xs =
  String.concat "\n" xs

let is_empty_string s =
  s = ""

let is_empty_list xs =
  xs = []

let is_some = function
  | None -> false
  | _ -> true

let string_of_char c =
  String.make 1 c

let list_group ~break xs =
  let add y ys = (y :: List.hd ys) :: (List.tl ys) in
  let rec lg xs acc =
    match xs with
    | [] -> acc
    | y :: ys ->
      match acc with
      | [] -> lg ys [[y]]
      | h :: t -> begin
        if break y (List.hd h) then
          lg ys ([y] :: acc)
        else
          lg ys (add y acc)
      end
  in
  let out = lg xs [] in
  List.map List.rev out |> List.rev
