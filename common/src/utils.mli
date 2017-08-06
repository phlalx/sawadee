

val parse_uri : string -> 
  [ `Magnet of string | `File of string | `Invalid_magnet | `Other ]
