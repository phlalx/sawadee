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

exception Wrong_Format 

(* TODO factorize get and split with other Torrent module *)
let get x =
  match x with
  | Some y -> y
  | None -> raise Wrong_Format

let split (s:string) split_size =
  let n = String.length s in
  assert (n % split_size = 0);
  let f i = String.sub s (i * split_size) split_size in
  Array.init (n / split_size) ~f

type tracker_reply = {
  complete : int;
  incomplete : int;
  interval : int;
  peers : Socket.Address.Inet.t list
}

let rec decode_peers s =
  let ar = split s 6 in
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
