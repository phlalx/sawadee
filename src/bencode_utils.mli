(** Utility functions used for extracting bencode *)

open Core
open Async

exception Bencode_error

val get : 'a option -> 'a

val peer_to_bencode : Socket.Address.Inet.t -> Bencode.t

val peers_to_bencode : Socket.Address.Inet.t list -> Bencode.t

val bencode_to_peer : Bencode.t -> Socket.Address.Inet.t 

val bencode_to_peers : Bencode.t -> Socket.Address.Inet.t list

val node_to_bencode : Node_id.t -> Bencode.t

val nodes_to_bencode : Node_id.t list -> Bencode.t

val bencode_to_nodes : Bencode.t -> Node_id.t list 

val hash_to_bencode : Bt_hash.t -> Bencode.t

val hashs_to_bencode : Bt_hash.t list -> Bencode.t

val get_string_from_dict_exn : Bencode.t -> string -> string

val get_dict_from_dict_exn : Bencode.t -> string -> Bencode.t 

val get_int_from_dict_exn : Bencode.t -> string -> int

val get_list_from_dict_exn : Bencode.t -> string -> Bencode.t list