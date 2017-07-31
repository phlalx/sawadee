open Core
open Async

module B = Bencode_ext

type node_info = Node_id.t * Addr.t 

exception Bencode_error

let split (s:string) split_size =
  let n = String.length s in
  assert (n % split_size = 0);
  let f i = String.sub s (i * split_size) split_size in
  Array.init (n / split_size) ~f

let split_list (s:string) split_size =
  Array.to_list (split s split_size)


let peer_to_bencode peer_addr = 
  B.String (Addr.to_compact peer_addr)

let bencode_to_peer b : Addr.t =
  B.as_string_exn b |> Addr.of_compact 

let rec bencode_to_peers b =
  let s : string = B.as_string_exn b in
  let peer_addr_length = 6 in
  split s peer_addr_length
  |> Array.map ~f:Addr.of_compact
  |> Array.to_list

let rec bencode_list_to_peers b =
  B.as_list_exn b |> List.map ~f:bencode_to_peer

let peers_to_bencode peers = 
  let f acc p = acc ^ (Addr.to_compact p) in
  B.String (List.fold peers ~init:"" ~f)

let peers_to_bencode_list peers = List.map peers ~f:peer_to_bencode |> B.List

let node_to_bencode n = B.String (Node_id.to_string n)

let nodes_to_bencode l = B.String (String.concat (List.map l ~f:Node_id.to_string))

let hashs_to_bencode l = assert false

let hash_to_bencode h = B.String (Bt_hash.to_string h)

let bencode_to_nodes b = 
  let s = B.as_string_exn b in
  Array.map (split s Node_id.length) ~f:Node_id.of_string 
  |> Array.to_list

let node_info_to_string (n, p) = 
  (Node_id.to_string n) ^ (Addr.to_compact p)

let string_to_node_info s = 
  let s1 = String.sub s ~pos:0 ~len:Node_id.length in
  let s2 = String.sub s ~pos:Node_id.length ~len:6 in 
  (Node_id.of_string s1, Addr.of_compact s2) 

let nodes_info_to_bencode nis = 
  let f acc p = acc ^ (node_info_to_string p) in
  B.String (List.fold nis ~init:"" ~f)

let bencode_to_nodes_info b =
  let s : string = B.as_string_exn b in
  let node_info_length = Node_id.length + 6 in
  split s node_info_length
  |> Array.map ~f:string_to_node_info
  |> Array.to_list





