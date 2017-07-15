open Core
open Async
open Log.Global

(*
This is the request sent by mu-torrent for ubuntu-17.04-desktop-amd64.iso.torrent

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
  announce : string;
  announce_list : string list list;
  info_hash : Bt_hash.t;
  peer_id : Peer_id.t;
  port : string;
  uploaded : string;
  downloaded : string;
  event : string;
  left : string;
  compact : string;
}

let t = ref None

let init ~announce ~announce_list info_hash ~len peer_id =
  let left = string_of_int len in
  let port = "6969" in
  let uploaded = "0" in
  let downloaded = "0" in
  let event = "started" in 
  let compact = "1" in
  t := Some { announce; info_hash; left; peer_id; port; uploaded; downloaded;
              event; compact; announce_list }

let extract_list_of_peers s =
  try 
    let tr = Extract_bencode.from_tracker_reply s in
    Ok tr.Extract_bencode.peers
  with
  | ex -> Error ex


let query_tracker uri =
  try_with (fun () -> Cohttp_async.Client.get uri)
  >>= function 
  | Ok (_, body)  -> (
      Cohttp_async.Body.to_string body 
      >>= fun s ->
      return (extract_list_of_peers s)
      >>| function 
      | Ok res -> Some res
      | Error err -> None
    )
  | Error err -> return None

(* TODO we query the trackers in sequence, would by better in parallel *)
let rec query_all_trackers uris =
  match uris with
  | uri :: t -> (
    debug "trying %s" (Uri.to_string uri);
    query_tracker uri 
    >>= function 
    | Some res -> return (Some res)
    | None -> query_all_trackers t)
  | [] -> return None 

let query () =
  (* We follow roughly http://bittorrent.org/beps/bep_0012.html
     to deal with announce-list, but not quite exactly TODO *)
  let t = Option.value_exn !t in
  let announces =  
    match t.announce_list with 
    | [] -> [t.announce]
    | x -> List.fold x ~init:[] ~f:(@) (* quick and dirty flattening *) in 
  let params = 
    [("info_hash", Bt_hash.to_string t.info_hash); 
     ("peer_id", Peer_id.to_string t.peer_id); 
     ("port", t.port);
     ("uploaded", t.uploaded);
     ("downloaded", t.downloaded);
     ("event", t.event);
     ("left", t.left);
     ("compact", t.compact );
    ] in

  let create_uri_with_query (x:string) = 
    let uri = Uri.of_string x in Uri.with_query' uri params in

  let uris_with_parameters = List.map announces ~f:create_uri_with_query in

  query_all_trackers (List.permute uris_with_parameters)
