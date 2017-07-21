(** Utility functions used for extracting bencode *)
open Core

exception Bencode_error

val get : 'a option -> 'a

val split : string -> int -> string Array.t
