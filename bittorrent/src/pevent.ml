open Core

type t = 
  | Support_meta
  | Tinfo of Torrent.info
  | Bye
  | Piece of int
[@@deriving sexp]

let to_string e = Sexp.to_string (sexp_of_t e)