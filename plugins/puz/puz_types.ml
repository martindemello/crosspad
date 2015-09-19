exception PuzzleFormatError of string

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
  fileversion: string;
  scrambled_cksum: int;
  fill: string;
  solution: string;
  clues: string list;
  notes: string;
  extensions: extension list;
  puzzletype: puzzle_type;
  solution_state: solution_state;
  helpers: string list
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
  fileversion = "1.3";
  scrambled_cksum = 0;
  fill = "";
  solution = "";
  clues = [];
  notes = "";
  extensions = [];
  puzzletype = `Normal;
  solution_state = `Unlocked;
  helpers = [];
}
