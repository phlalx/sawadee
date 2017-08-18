open Core

type t = { piece : int; off: int; len: int }
[@@deriving sexp]

let to_string t = Sexp.to_string (sexp_of_t t)
