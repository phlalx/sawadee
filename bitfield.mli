(** Bittorrent bitfield. Alias for string. *)
open Core

type t
[@@deriving sexp]

val length : t -> int

val of_string : string -> t

val to_string : t -> string
