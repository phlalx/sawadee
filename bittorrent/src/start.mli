(* download a torrent or a magnet.

   For magnets, we find the peers via DHT only. If we don't have metainformation
   from previous sessions, we query the peers to get it.

   For torrent, we get the peers both from DHT (if enabled) and from the tracker.

   After we have metainformation, both process are identical and try to get 
   all the pieces. *)

open Core
open Async

(** [process_torrent s] downloads torrent given in [s] as bencode. Both
    function returns the info hash of the torrent used as a key in the 
    globlal [Torrent_table].a *)
val process_torrent : string -> Bt_hash.t

val process_magnet : Bt_hash.t -> Bt_hash.t