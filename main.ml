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

let start_app_layer info_hash peer_addrs file peer_id =
  let al = App_layer.create info_hash file peer_id in
  
  (* register handler for ctrl-c *)
  Signal.handle Signal.terminating ~f:(fun _ -> App_layer.stop al);
  App_layer.start al;
  Clock.every (sec 10.0) (fun () -> App_layer.stats al);
  let f = App_layer.add_peer al in
  List.iter peer_addrs ~f


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

  info "Torrent: %s:" torrent_name;
  info "Torrent: %d files" (List.length files_info);
  info "Torrent: %d pieces" (Array.length pieces_hash);
  info "Torrent: piece length = %d" piece_length;

  (* Create File.t and retrieve peristent data *)
  File.create pieces_hash ~torrent_name ~piece_length files_info 
  >>= fun file ->

  let total_length = File.length file in

  Tracker_client.init announce announce_list info_hash total_length G.peer_id; 
  debug "trying to connect to tracker";
  Tracker_client.query ()
  >>= function
  | Some peer_addrs -> 
    let num_of_peers = List.length peer_addrs in 
    info "tracker replies with list of %d peers" num_of_peers;
    return (start_app_layer info_hash peer_addrs file G.peer_id)
  | None -> 
    info "can't connect to tracker";
    flushed ()
    >>= fun () ->
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



