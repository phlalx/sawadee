(** Communication with peers.

    [t] is used to handshake with peers then communicate via [Message.t] 
    send/receive.

    We distinguish between the peers that initiate the connexion (we get them
    via [Server]) and those we contact from their address (e.g. from tracker 
    or DHT). This makes a difference on who initiate handshake.

    Note that we handshake before adding peers to the swarm as in the case of
    server peers, we don't know what torrents they wish to download before
    handhake *)

open Core
open Async

type t 

val create : Addr.t -> Reader.t -> Writer.t -> t

val create_with_connect : Addr.t -> t Deferred.Or_error.t

val to_string : t -> string

type handshake_info = {
    extension : bool;
    dht : bool;
    info_hash : Bt_hash.t;
    peer_id : Peer_id.t
}

val downloaded : t -> int

val uploaded : t -> int

val download_speed : t -> float

val upload_speed : t -> float

val initiate_handshake: t -> Bt_hash.t -> handshake_info Deferred.Or_error.t

val wait_handshake : t -> (Bt_hash.t -> bool) -> 
   handshake_info Deferred.Or_error.t

val receive : t -> Message.t Reader.Read_result.t Deferred.t

val send : t -> Message.t -> unit

val addr : t -> Unix.Inet_addr.t

(* close fd *)
val close : t -> unit Deferred.t

