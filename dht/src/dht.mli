(** Simple DHT service.

The official specification http://bittorrent.org/beps/bep_0005.html  lacks
implementation details. The most detailed description of the protocol I found
is the specification of xlattice.
http://xlattice.sourceforge.net/components/protocol/kademlia/specs.html

The current implementation is partial. Lookup works well enough but the 
bucket mechanisms and the tokens aren't implemented yet.

TODO: finish the implementation by following xlattice spec. Complete integration 
test [test_dht.ml]. *)

open Core
open Async

type t

val create : ?token_time:Time.Span.t ->  port:int -> Node_id.t ->
  data_path:string -> verbose:int -> t

(* try to add an address to the table. Ping this address and if it's 
   a node, add it. *)
val try_add : t -> Addr.t -> unit Deferred.Or_error.t

val table : t -> Node_info.t list

val lookup : t -> Bt_hash.t -> Addr.t list Deferred.t

val announce : t -> Bt_hash.t -> port:int -> unit

val to_string : t -> string
