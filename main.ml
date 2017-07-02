open Core
open Async
open Log.Global

let process f = 
  let c = In_channel.create f in 
  let open Extract_bencode in 
  let { info_sha1; announce; length; pieces } = Extract_bencode.from_torrent c in
  let file = File.create length info_sha1 pieces in
  Tracker_client.init announce info_sha1 length; 
  Tracker_client.query ()
  >>= fun peers ->
  match peers with
  | peer :: _ -> 
    (* let peer = Unix.Inet_addr.localhost, 8080 in *)
    Peer.start (Peer.create peer file) (* consider only first peer *)
  | [] -> assert false

let spec =
  let open Command.Spec in
  empty
  +> anon ("FILE" %: string)

let command =
  Command.basic
    ~summary:"Download torrent file"
    spec
    (fun filename -> (fun () -> ignore(process filename)))


let () = 
  set_level `Debug;
  Command.run command;
  never_returns (Scheduler.go ())

