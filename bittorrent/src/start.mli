(* download a torrent or a magnet.

   For magnets, we find the peers via DHT only. If we don't have metainformation
   from previous sessions, we query the peers to get it.

   For torrents, we get the peers both from DHT (if enabled) and from the 
   tracker.

   One we have the meta-info, both functions behave identically and try
   to download all the pieces *)

open Core
open Async

(** [process_torrent s] downloads torrent given in [s] as bencode. Both
    function returns immediately with the info hash of the torrent used 
    as a key in the globlal [Torrent_table]. One can see the status of the
    download with [Bittorrent.status] *)
val process_torrent : string -> Bt_hash.t

val process_magnet : Bt_hash.t -> Bt_hash.t