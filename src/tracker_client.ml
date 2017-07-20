open Core
open Async
open Log.Global

module B = Bencode

(* TODO don't forget to send port when -l is set *)
type t = {
  announce : string;
  announce_list : string list list;
  info_hash : Bt_hash.t;
  peer_id : Peer_id.t;
  uploaded : string;
  downloaded : string;
  event : string;
  left : string;
  compact : string;
}

let t = ref None

let init ~announce ~announce_list info_hash ~len peer_id =
  let left = string_of_int len in
  let uploaded = "0" in
  let downloaded = "0" in
  let event = "started" in 
  let compact = "1" in
  t := Some { announce; info_hash; left; peer_id; uploaded; downloaded;
              event; compact; announce_list }


let query_tracker uri =
  match%bind try_with (fun () -> Cohttp_async.Client.get uri) with
  | Ok (_, body)  -> (
      let%bind s = Cohttp_async.Body.to_string body in
      let r = Tracker_reply.from_bencode s in
      return (Some r.Tracker_reply.peers)
    )
  | Error err -> return None

(* TODO we return any tracker that returns an answer, not quite the spec *)
let rec query_all_trackers uris =
  match%bind Deferred.List.filter_map uris ~how:`Parallel ~f:query_tracker with 
  | res :: _ -> return (Ok res)
  | [] -> return (Error (Error.of_string "can connect to tracker"))

let query () =
  let t = Option.value_exn !t in
  let announces =  
    match t.announce_list with 
    | [] -> [t.announce]
    | x -> List.fold x ~init:[] ~f:(@) (* quick and dirty flattening *) in 
  let params = 
    [("info_hash", Bt_hash.to_string t.info_hash); 
     ("peer_id", Peer_id.to_string t.peer_id); 
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
