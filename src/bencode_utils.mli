(** Utility functions used for extracting bencode *)

open Core
open Async

exception Bencode_error

module B = Bencode_ext

val split_list : string -> int -> string list

val peer_to_bencode : Addr.t -> B.t

val peers_to_bencode : Addr.t list -> B.t

val peers_to_bencode_list : Addr.t list -> B.t

(* TODO be consistent with name of these conversion functions *)
val peer_to_string :  Addr.t -> string 

val string_to_peer :  string -> Addr.t  

val bencode_to_peer : B.t -> Addr.t 

val bencode_to_peers : B.t -> Addr.t list

val bencode_list_to_peers : B.t -> Addr.t list

val node_to_bencode : Node_id.t -> B.t

val nodes_to_bencode : Node_id.t list -> B.t

val nodes_info_to_bencode : (Node_id.t * Addr.t) list ->
  B.t

val bencode_to_nodes_info : B.t ->
  (Node_id.t * Addr.t) list

val bencode_to_nodes : B.t -> Node_id.t list 

val hash_to_bencode : Bt_hash.t -> B.t

val hashs_to_bencode : Bt_hash.t list -> B.t
