(* Data client/server tracker interaction.

  This is answered by the server to the client as bencode.


  TODO: need to be more permissive in what we accept as reply *)
 
open Core

type ok = { interval : int; peers : Addr.t list }

type t = (ok, string) Result.t

val of_string : string -> t

val to_string : t -> string
 