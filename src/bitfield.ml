open Core
(* open Sexplib.Std *)

type t = string
[@@deriving sexp]

let length = String.length

let to_string x = x

let of_string x = x  

let empty n = String.make n '\000'