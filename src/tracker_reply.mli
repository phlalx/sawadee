open Core
open Async

exception Bencode_error

type t = {
  complete : int;
  incomplete : int;
  interval : int;
  peers : Socket.Address.Inet.t list
}

val from_bencode : string -> t

val to_bencode : t -> string
 