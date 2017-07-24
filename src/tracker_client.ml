open Core
open Async
open Log.Global

module B = Bencode

(* TODO fill this correctly *)
let uploaded = "0"
let downloaded = "0" 
let event = "started"
let compact = "1" 

let create_uri_with_parameters t (s:string) = 
  let open Torrent in
  let params_base = 
    [("info_hash", Bt_hash.to_string t.info_hash); 
     ("peer_id", Peer_id.to_string Global.peer_id); 
     ("uploaded", uploaded);
     ("downloaded", downloaded);
     ("event", event);
     ("left", string_of_int t.total_length);
     ("compact", compact );
    ] 
  in
  let params = 
    if Global.is_server () then
      let port = string_of_int (Global.port_exn ()) in
      ("port", port) :: params_base
    else
      params_base
  in 
  Uri.with_query' (Uri.of_string s) params 

type sl = Socket.Address.Inet.t list 

(* Returns list of peers from tracker or error. 

    Error can come from
    - failed connecting attempt
    - server can't process the request 
    - error while decoding the reply *)
let query_tracker t uri : sl Deferred.Option.t = 
  let uri = create_uri_with_parameters t uri in
  info "trying uri %s" (Uri.to_string uri);
  let reply_or_error : sl Deferred.Or_error.t =
    let open Deferred.Or_error.Monad_infix in
    Deferred.Or_error.try_with (fun () -> Cohttp_async.Client.get uri)
     >>= fun (response, body) -> 
     (* let http_code = Cohttp.Response.status response in *)
     (* TODO check response.status = `OK; *)
     Deferred.ok (Cohttp_async.Body.to_string body)
     >>= fun s ->
     return (Or_error.try_with (fun () -> Tracker_reply.of_bencode s))
     >>= fun t ->
     return (Ok t.Tracker_reply.peers)  
  in
  match%bind reply_or_error with
  | Ok l -> return (Some l)
  | Error l -> return None

let query_all_trackers t uris =
  let uris = List.take uris 1 in 
  match%bind Deferred.List.filter_map uris ~how:`Parallel ~f:(query_tracker t) with 
  | res :: _ -> return (Some res)
  | [] -> return None

let query t =
  let uris = 
    match t.Torrent.announce_list with
    | [] -> [ t.Torrent.announce ]
    | al -> List.dedup (List.concat al) in 
  let permuted_uris = List.permute uris in
  query_all_trackers t permuted_uris 










