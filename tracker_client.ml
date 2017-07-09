open Core
open Async
open Log.Global

(*

This is the request sent by mutorrent for ubuntu-17.04-desktop-amd64.iso.torrent

GET /announce?info_hash=Y%06gi%b9%adB%da.P%86%11%c3%3d%7cD%80%b3%85%7b&peer_id=-UM1870-%b1%a5g%15%ec%08%89%0cg%cb%bf%f6&port=61137&uploaded=0&downloaded=0&left=1609039872&corrupt=0&key=31C3ABE8&event=started&numwant=200&compact=1&no_peer_id=1&ipv6=fe80%3a%3a443%3aef89%3ab70e%3ac7a1 HTTP/1.1
 info_hash=Y%06gi%b9%adB%da.P%86%11%c3%3d%7cD%80%b3%85%7b&
 peer_id=-UM1870-%b1%a5g%15%ec%08%89%0cg%cb%bf%f6&
 port=61137&
 uploaded=0&
 downloaded=0&
 left=1609039872&
 corrupt=0&
 key=31C3ABE8&
 event=started&
 numwant=200&
 compact=1&
 no_peer_id=1&
 ipv6=fe80%3a%3a443%3aef89%3ab70e%3ac7a1 
*)

type t = {
  mutable announce : string;
  mutable info_sha1 : string;
  mutable peer_id : string;
  mutable port : string;
  mutable uploaded : string;
  mutable downloaded : string;
  mutable event : string;
  mutable key : string;
  mutable left : string;
}

let st = {  (* TODO: this is partly based on mu-torrent query. See spec to
               use proper values *)
  announce = ""; 
  info_sha1 = ""; 
  peer_id = "31234";
  port = "6969";
  uploaded = "0";
  downloaded = "0";
  event = "started";
  key = "31C3ABE8";
  left = ""
}


let init ~announce ~info_sha1 ~length ~peer_id =
  st.announce <- announce;
  st.info_sha1 <- info_sha1;
  st.left <- string_of_int length;
  st.peer_id <- peer_id

let extract_list_of_peers s =
    try 
      let tr = Extract_bencode.from_tracker_reply s in
      Ok tr.Extract_bencode.peers
    with
    | ex -> Error ex

let query () =
  let uri = Uri.of_string st.announce in
  let params = 
    [("info_hash", st.info_sha1); 
     ("peer_id", st.peer_id); 
     ("port", st.port);
     ("uploaded", st.uploaded);
     ("downloaded", st.downloaded);
     ("event", st.event);
     ("key", st.key); 
     ("left", st.left);
     ("compact", "1");
    ] in
  let uri_with_query = Uri.with_query' uri params in
  try_with (fun () -> Cohttp_async.Client.get uri_with_query)
  (* TODO is there a way to make this look cleaner *)
  >>= function 
  | Ok (_, body)  -> (
    Cohttp_async.Body.to_string body 
    >>= fun s ->
    return (extract_list_of_peers s)
    >>= function 
    | Ok res -> return (Ok res)
    | Error err -> return (Error err)
  )
  | Error err -> return (Error err)
















