open Core
open Async

type t = 
  | Reject 
  | Data of string 
  | Handshake of int * int
    (* specify the message identifier for metadata and metadata size *)
  | Unknown

val to_string : t -> string

val of_string : string -> t