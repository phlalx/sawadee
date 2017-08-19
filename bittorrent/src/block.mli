open Core

type t = { piece : int; off: int; len: int }
[@@deriving sexp]

val to_string : t -> string