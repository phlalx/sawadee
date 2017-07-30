open Core
open Async
open Log.Global

module B = Bencode_ext

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

let check_response response =
  let error = Error.of_string "Status not OK from tracker" in
  let status = Cohttp.Response.status response in
  let is_status_ok = status = `OK in
  if not is_status_ok then
    info "http response not ok";
  return (Result.ok_if_true is_status_ok ~error)

let check_scheme uri =
  let error = Error.of_string "URI scheme is not http" in
  let scheme = Uri.scheme uri in
  let is_scheme_http = match scheme with 
    | Some s -> s = "http"
    | None -> false
  in 
  if not is_scheme_http then
    info "uri scheme not http";
  return (Result.ok_if_true is_scheme_http ~error)

(*  Error can come from
    - failed connecting attempt
    - server can't process the request 
    - error while decoding the reply *)
let query_tracker t uri : sl Deferred.Option.t = 
  let uri = create_uri_with_parameters t uri in
  debug "trying uri %s" (Uri.to_string uri);
  let reply_or_error : sl Deferred.Or_error.t =
    let open Deferred.Or_error.Monad_infix in
    check_scheme uri 
    >>= fun () ->
    Deferred.Or_error.try_with (fun () -> Cohttp_async.Client.get uri)
    >>= fun (response, body) -> 
    check_response response
    >>= fun () ->
    Deferred.ok (Cohttp_async.Body.to_string body)
    >>= fun s ->
    return (Or_error.try_with (fun () -> Tracker_reply.of_bencode s))
    >>= fun t ->
    return (Ok t.Tracker_reply.peers)  
  in
  reply_or_error >>| Result.ok 

let query_all_trackers t uris =
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

