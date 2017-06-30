open Core
open Async
open Log.Global

(*
GET /announce?info_hash=Y%06gi%b9%adB%da.P%86%11%c3%3d%7cD%80%b3%85%7b&peer_id=-UM1870-%b1%a5g%15%ec%08%89%0cg%cb%bf%f6&port=61137&uploaded=0&downloaded=0&left=1609039872&corrupt=0&key=31C3ABE8&event=started&numwant=200&compact=1&no_peer_id=1&ipv6=fe80%3a%3a443%3aef89%3ab70e%3ac7a1 HTTP/1.1
*)

let query ~announce ~info_sha1 =
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