val read : string -> Xword.Types.xword
val write : Xword.Types.xword -> string
val write_json : Xword.Types.xword -> Ezjsonm.t
val error : string -> Ezjsonm.t
