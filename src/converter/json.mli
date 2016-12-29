val read : string -> Typedefs.xword
val write : Typedefs.xword -> string
val write_json : Typedefs.xword -> Ezjsonm.t
val error : string -> Ezjsonm.t
