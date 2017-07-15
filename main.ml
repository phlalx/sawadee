(** Entry point to the project. It implements the command-line options and 
    run the various stages. So far, it includes:
      - decoding bencoded torrent file
      - querying the tracker to retrieve list of peers
      - initiating the P2P protocol with one of the peers.

    All this project makes use of [Core], [Async] and [Log.Global]. *)

open Core
open Async
open Log.Global

let start_app_layer peer_addrs file peer_id =
  let al = App_layer.create file peer_id in
  (* DEBUG ONLY, keep only a subset of peers *) 
  (* let peer_addrs = List.sub peer_addrs ~pos:0 ~len:5 in  *)
  App_layer.start al;
  List.iter ~f:(App_layer.add_peer al) peer_addrs

(** [process f] initiates downloading of file described by
    torrent file named [f]. *)
let process (f : string)  = 
  let c = In_channel.create f in 
  let open Extract_bencode in 
  let { info_hash; announce; announce_list; mode; pieces_hash; piece_length; 
        files_info }
    = Extract_bencode.from_torrent c in
  (* TODO ignore file names until we actually write file to disk *)
  let { name; _ } = List.hd_exn files_info in 
  let length = List.fold files_info ~init:0 ~f:(fun acc x -> acc + x.length) in
  File.create ~len:length info_hash pieces_hash ~piece_length ~name 
  >>= fun file ->
  let peer_id = Peer_id.random () in
  Tracker_client.init announce announce_list info_hash length peer_id; 
  debug "trying to connect to tracker";
  Tracker_client.query ()
  >>= function
  | Some peer_addrs -> 
    info "tracker replies with list of %d peers" (List.length peer_addrs);
    return (start_app_layer peer_addrs file peer_id)
  | None -> 
    info "can't connect to tracker";
    flushed ()
    >>= fun () ->
    exit 1

let spec =
  let open Command.Spec in
  empty
  +> anon ("FILE" %: string)

let command =
  Command.basic
    ~summary:"Download torrent file"
    spec
    (fun filename -> (fun () -> Deferred.don't_wait_for (process filename)))

let () = 
  set_level `Info;
  Command.run command;
  never_returns (Scheduler.go ())



















