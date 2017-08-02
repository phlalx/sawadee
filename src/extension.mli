open Core
open Async

type ext = [ `Metadata of int * int ]

type t = 
  | Reject of int  
  | Request of int 
  | Data of int * string 
  | Handshake of ext list
  | Unknown

val to_bin : t -> string

val of_bin : string -> t

val to_string : t -> string