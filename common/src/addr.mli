open Core
open Async

include (module type of Socket.Address.Inet)

val to_compact : t -> string 

val of_compact : string -> t

val to_bencode : t -> Bencode_ext.t

val of_bencode : Bencode_ext.t -> t

val list_to_bencode : t list -> Bencode_ext.t

val list_of_bencode : Bencode_ext.t -> t list

val list_to_bencode_list : t list -> Bencode_ext.t

val list_of_bencode_list : Bencode_ext.t -> t list

val length : int

val of_string : string -> t

val is_valid : t -> bool