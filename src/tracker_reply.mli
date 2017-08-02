(* Data client/server tracker interaction.

  This is answered by the server to the client as bencode.


  TODO: need to be more permissive in what we accept as reply *)
 
open Core
open Async

type t = {
  complete : int;
  incomplete : int;
  interval : int;
  peers : Addr.t list
}

val of_bencode : string -> t

val to_bencode : t -> string
 