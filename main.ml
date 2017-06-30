open Core
open Async
open Log.Global

let process f = 
  let c = In_channel.create f in 
  let open Extract_bencode in 
  let { info_sha1; announce; length } = Extract_bencode.from_torrent c in
  Tracker_client.init announce info_sha1 length; 
  Tracker_client.query ()

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

