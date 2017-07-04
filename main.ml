(** Entry point to the project. It implements the command-line options and 
    run the various stages. So far, it includes:
      - decoding bencoded torrent file
      - querying the tracker to retrieve list of peers
      - initiating the P2P protocol with one of the peers.

    All this project makes use of [Core], [Async] and [Log.Global]. *)

open Core
open Async
open Log.Global

(** [process f] initiates downloading of file described by
    torrent file named [f]. *)
let process (f : string)  = 
  let c = In_channel.create f in 
  let open Extract_bencode in 
  let { info_sha1; announce; length; pieces } = Extract_bencode.from_torrent c in
  let file = File.create ~len:length ~sha:info_sha1 ~pieces in
  Tracker_client.init announce info_sha1 length; 
  Tracker_client.query ()
  >>= fun peer_addrs ->
  (* to make it simpler, we deal with only one peer *)
  let peer_addr = List.hd_exn peer_addrs in 
  Peer.create peer_addr file 
  >>= function 
  | None -> return(debug "can't connect to host")
  | Some p -> 
    Peer.handshake p >>= function
    | None -> return(debug "handshake failed")
    | Some p -> exit 0

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
  set_level `Debug;
  Command.run command;
  (* Deferred.don't_wait_for (Tests.test ()); *)
  never_returns (Scheduler.go ())












