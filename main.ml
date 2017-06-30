open Core
open Async
open Log.Global


let process f = 
  let c = In_channel.create f in 
  let open Extract_bencode in 
  let { name; info_sha1; announce; pieces } = extract_from_bencode c in
  Tracker_client.query announce info_sha1  

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

