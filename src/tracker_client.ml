open Core
open Async
open Log.Global

module B = Bencode

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


type tracker_reply = {
  complete : int;
  incomplete : int;
  interval : int;
  peers : Socket.Address.Inet.t list
}

let rec decode_peers s =
  let peer_addr_length = 6 in
  let ar = Bencode_utils.split s peer_addr_length in
  let compact_repr (s:string) : Socket.Address.Inet.t =
    let addr_int32 = Binary_packing.unpack_signed_32 ~byte_order:`Big_endian 
        ~buf:s ~pos:0 in
    let port = Binary_packing.unpack_unsigned_16_big_endian ~pos:4 ~buf:s in
    let addr = Unix.Inet_addr.inet4_addr_of_int32 addr_int32 in
    Socket.Address.Inet.create addr port
  in 
  Array.to_list (Array.map ar ~f:compact_repr)

let extract_bencode s =
  let bc = B.decode (`String s) in 
  debug "Tracker reply = %s" (B.pretty_print bc);
  let open Bencode_utils in
  let complete = get ((B.as_int (get (B.dict_get bc "complete")))) in
  let incomplete = get ((B.as_int (get (B.dict_get bc "incomplete")))) in
  let interval = get ((B.as_int (get (B.dict_get bc "interval")))) in
  let peers_str = get (B.as_string (get (B.dict_get bc "peers"))) in
  let peers = decode_peers peers_str in
  { complete; incomplete; interval; peers; }

let extract_list_of_peers s =
  try 
    let tr = extract_bencode s in
    Ok tr.peers
  with
  | ex -> Error ex


let query_tracker uri =
  match%bind try_with (fun () -> Cohttp_async.Client.get uri) with
  | Ok (_, body)  -> (
      let%bind s = Cohttp_async.Body.to_string body in
      return (extract_list_of_peers s)
      >>| function 
      | Ok res -> Some res
      | Error err -> None
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
