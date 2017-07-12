(** Entry point to the project. It implements the command-line options and 
    run the various stages. So far, it includes:
      - decoding bencoded torrent file
      - querying the tracker to retrieve list of peers
      - initiating the P2P protocol with one of the peers.

    All this project makes use of [Core], [Async] and [Log.Global]. *)

open Core
open Async
open Log.Global

let random_id () = 
  String.init 20 ~f:(fun _ -> char_of_int (Random.int 255))

(** [process f] initiates downloading of file described by
    torrent file named [f]. *)
let process (f : string)  = 
  let c = In_channel.create f in 
  let open Extract_bencode in 
  let { info_hash; announce; length; pieces_hash; name; piece_length }
    = Extract_bencode.from_torrent c in
  let file = File.create ~len:length ~hash:info_hash ~pieces_hash ~piece_length ~name 
  in

  let this_peer_id = random_id () in
  Tracker_client.init announce info_hash length this_peer_id; 
  info "trying to connect to tracker";
  Tracker_client.query ()
  >>= function
  | Ok peer_addrs ->
      info "got list of peers";
      let al = App_layer.create file this_peer_id in
      let peer_addrs = [List.hd_exn peer_addrs] in (* DEBUG ONLY, keep one peer *)
      App_layer.start al peer_addrs
  | Error exn -> 
    info "can't connect to tracker";
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
  (* Deferred.don't_wait_for (Tests.test ()); *)
  never_returns (Scheduler.go ())


