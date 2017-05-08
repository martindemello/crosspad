open Typedefs

let string_of_metadata_key (k : [< metadata_key]) = match k with
| `Annotation -> "Annotation"
| `Author -> "Author"
| `Block -> "Block"
| `Copyright -> "Copyright"
| `Date -> "Date"
| `Difficulty -> "Difficulty"
| `Editor -> "Editor"
| `Empty -> "Empty"
| `Explanation -> "Explanation"
| `Intro -> "Intro"
| `Notes -> "Notes"
| `Origin -> "Origin"
| `Publication -> "Publication"
| `Publisher -> "Publisher"
| `Styles -> "Styles"
| `Title -> "Title"
| `Uniqueid -> "Uniqueid"
| `Url -> "Url"
| `Unknown -> "UNRECOGNISED METADATA"

let metadata_key_of_string k = match k with
| "Annotation" -> `Annotation
| "Author" -> `Author
| "Block" -> `Block
| "Copyright" -> `Copyright
| "Date" -> `Date
| "Difficulty" -> `Difficulty
| "Editor" -> `Editor
| "Empty" -> `Empty
| "Explanation" -> `Explanation
| "Intro" -> `Intro
| "Notes" -> `Notes
| "Origin" -> `Origin
| "Publication" -> `Publication
| "Publisher" -> `Publisher
| "Styles" -> `Styles
| "Title" -> `Title
| "Uniqueid" -> `Uniqueid
| "Url" -> `Url
| _ -> `Unknown

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
