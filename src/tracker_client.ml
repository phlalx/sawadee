open Core
open Async
open Log.Global

module B = Bencode

type t = {
  announce : string;
  announce_list : string list list;
  info_hash : Bt_hash.t;
  uploaded : string;
  downloaded : string;
  event : string;
  left : string;
  compact : string;
}

let tc = ref None

let init (t : Torrent.t) =
  let open Torrent in
  let { 
    total_length;
    info_hash;
    announce_list;
    announce
  } = t in
  let left = string_of_int total_length in
  let uploaded = "0" in
  let downloaded = "0" in
  let event = "started" in 
  let compact = "1" in
  tc := Some { announce; info_hash; left; uploaded; downloaded; event; compact; 
              announce_list = t.announce_list }


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
  let t = Option.value_exn !tc in
  let announces =  
    match t.announce_list with 
    | [] -> [t.announce]
    | x -> List.fold x ~init:[] ~f:(@) (* quick and dirty flattening *) in 
  let params_base = 
    [("info_hash", Bt_hash.to_string t.info_hash); 
     ("peer_id", Peer_id.to_string Global.peer_id); 
     ("uploaded", t.uploaded);
     ("downloaded", t.downloaded);
     ("event", t.event);
     ("left", t.left);
     ("compact", t.compact );
    ] in
  let params = 
    if Global.is_server () then
      let port = string_of_int (Global.port_exn ()) in
      ("port", port) :: params_base
    else
      params_base
  in
  let create_uri_with_query (x:string) = 
    let uri = Uri.of_string x in Uri.with_query' uri params in

  let uris_with_parameters = List.map announces ~f:create_uri_with_query in

  query_all_trackers (List.permute uris_with_parameters)
