open Core
(* open Sexplib.Std *)

type t = string
[@@deriving sexp]

let length = String.length

let to_string = ident

let of_string = ident

let empty n = String.make n '\000'