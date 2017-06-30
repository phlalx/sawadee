open Core
open Async
open Log.Global

let query_tracker announce info_sha1 =
  let uri = Uri.of_string announce in
  let uri_with_query = Uri.with_query' 
    uri 
    [("info_hash", info_sha1); ("peer_id", "123"); ("port", "9345")] 
  in
  debug "uri updated = %s" (Uri.to_string uri_with_query); 
  Cohttp_async.Client.get uri_with_query
  >>= fun (_, body) -> 
  Cohttp_async.Body.to_string body 
  >>= fun s ->
  return (debug "body = %s" s) 
  >>= fun () ->
  exit 0

let process f = 
  let c = In_channel.create f in 
  let open Extract_bencode in 
  let { name; info_sha1; announce; pieces } = extract_from_bencode c in
  query_tracker announce info_sha1  

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

