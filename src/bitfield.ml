open Core

type t = string
[@@deriving sexp]

let length = String.length

let to_string x = x

let of_string x = x