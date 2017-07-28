(** Utility functions used for extracting bencode *)

open Core
open Async

exception Bencode_error

val get : 'a option -> 'a

val split : string -> int -> string Array.t

val encode_peer : Socket.Address.Inet.t -> string 

val decode_peer : string -> Socket.Address.Inet.t 