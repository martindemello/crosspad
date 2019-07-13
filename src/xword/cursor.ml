type t = {
  x : int;
  y : int;
  xmax : int;
  ymax : int;
}

let make xmax ymax = { x = 0; y = 0; xmax; ymax }

let pin x max = if x < 0 then 0 else if x >= max then max - 1 else x

(* handle negative numbers properly *)
let cyc x max = (x + max) mod max

let delta = function
  | `Left -> (-1, 0)
  | `Right -> (1, 0)
  | `Up -> (0, -1)
  | `Down -> (0, 1)
  | `Across -> (1, 0)
  | `Bksp_Ac -> (-1, 0)
  | `Bksp_Dn -> (0, -1)
  | _ -> (0, 0)

let move cursor ?wrap:(wrap=true) dir =
  let x, y = delta dir in
  let x, y = cursor.x + x, cursor.y + y in
  let f = if wrap then cyc else pin in
    { cursor with x = f x cursor.xmax; y = f y cursor.ymax }

let set cursor x y =
  if x >= 0 && x < cursor.xmax && y >= 0 && y < cursor.ymax then
    { cursor with x; y }
  else
    cursor
