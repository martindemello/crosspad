(* Datatypes *)

type puzzle_type = [`Normal | `Diagramless]

type solution_state = [`Locked | `Unlocked]

type grid_markup = [`Default | `PreviouslyIncorrect | `Incorrect | `Revealed | `Circled]

type parsed_extension = [ `RTBL of (int * string) list
                        | `GRBS of string
                        | `GEXT of string
                        | `LTIM of (int * int)
                        ]

(* extension read in from binary *)
type extension = {
  section: string;
  length: int;
  data: string;
  checksum: int;
}

(* puzzle read in from binary *)
type puzzle = {
  preamble: string;
  postscript: string;
  title: string;
  author: string;
  copyright: string;
  width: int;
  height: int;
  n_clues: int;
  version: string;
  scrambled_checksum: int;
  fill: string;
  solution: string;
  clues: string list;
  notes: string;
  extensions: extension list;
  puzzle_type: int;
  solution_state: int;
  scrambled_tag: int;
}

let new_puzzle = {
  preamble = "";
  postscript = "";
  title = "";
  author = "";
  copyright = "";
  width = 0;
  height = 0;
  n_clues = 0;
  version = "1.3";
  scrambled_checksum = 0;
  fill = "";
  solution = "";
  clues = [];
  notes = "";
  extensions = [];
  puzzle_type = 1;
  solution_state = 0;
  scrambled_tag = 0;
}
