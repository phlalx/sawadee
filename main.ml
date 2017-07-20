(** Entry point to the project. 

    It implements the command-line options and run main stages. 

    - decoding the metainfo (torrent) file
    - creating File.t and retrieving persistent data
    - querying the tracker to retrieve list of peers
    - initiating the P2P layer
    - add peers.

    All modules use [Core], [Async] and [Log.Global]. *)

open Core
open Async
open Log.Global
module G = Global

(** [process f] downloads files described by metainfo file [f]. *)
let process (torrent_name : string)  = 

  (* decode file *)
  let open Torrent in 
  let { info_hash; 
        announce; 
        announce_list; 
        mode; 
        pieces_hash; 
        piece_length; 
        files_info }
    = Torrent.from_file torrent_name 
  in

  let num_pieces = Array.length pieces_hash in

  info "Torrent: %s:" torrent_name;
  info "Torrent: %d files" (List.length files_info);
  info "Torrent: %d pieces" num_pieces;
  info "Torrent: piece length = %d" piece_length;

  let total_length = List.fold files_info ~init:0 ~f:(fun acc (_,l) -> l + acc) in 

  Tracker_client.init announce announce_list info_hash total_length G.peer_id; 
  debug "trying to connect to tracker";
  match%bind Tracker_client.query () with
  | Ok peer_addrs -> 
    let num_of_peers = List.length peer_addrs in 
    info "tracker replies with list of %d peers" num_of_peers;
    let bitfield_name = (Filename.basename torrent_name) ^ G.bitset_ext in
    let bf_length = Bitset.bitfield_length_from_size num_pieces in 
    let%bind al = App_layer.create 
      info_hash 
      bitfield_name bf_length 
      files_info
      pieces_hash
      G.peer_id 
      piece_length
      total_length
    in
    let stop (_:Signal.t) = App_layer.stop al in
    (* register handler for ctrl-c *)
    Signal.handle Signal.terminating ~f:stop;
    App_layer.start al;
    let f = App_layer.add_peer al in
    List.iter peer_addrs ~f;
    Deferred.unit
  | Error err -> 
    info "can't connect to tracker";
    flushed () >>= fun () ->
    exit 1

let spec =
  let open Command.Spec in
  empty +> anon ("FILE" %: string) 

let command =
  Command.basic ~summary:"Download torrent file" spec
    (fun filename -> (fun () -> Deferred.don't_wait_for (process filename)))

let () = 
  set_level `Info;
  Command.run command;
  never_returns (Scheduler.go ())



