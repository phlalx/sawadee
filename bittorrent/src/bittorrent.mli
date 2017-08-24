(** Interface of the bittorrent library. *)

open Core
open Async

(** [create] checks that the parameters are valid and starts the servers. 
    It fails on error. Should be called before any operation *)

val create: 
  server_port:(int option) -> (* port for the incoming peers *)
  verbose:int ->     (* log level - 1 or 2 *)
  torrent_path:string ->      (* path to save meta-data *)
  download_path:string ->     (* path for downloaded files *)
  dht_port:(int option) ->    (* port for DHT server *)
  unit Deferred.t

(** download a torrent given as a string *)
val add_torrent : string -> Bt_hash.t

val add_magnet : string -> Bt_hash.t

(** take a file name and starts to seed it *)
val seed : string -> piece_length:int -> Bt_hash.t Or_error.t 

val torrent_list : unit -> Bt_hash.t list

val terminate : unit -> unit Deferred.t 

val status : Bt_hash.t -> Status.t option

