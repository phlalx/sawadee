open Core
open Async

module B = Bencode

exception Bencode_error

let get x =
  match x with
  | Some y -> y
  | None -> raise Bencode_error

let split (s:string) split_size =
  let n = String.length s in
  assert (n % split_size = 0);
  let f i = String.sub s (i * split_size) split_size in
  Array.init (n / split_size) ~f

let split_list (s:string) split_size =
    Array.to_list (split s split_size)

let peer_to_string peer_addr = 
  let port = Socket.Address.Inet.port peer_addr in
  let addr = Socket.Address.Inet.addr peer_addr in
  let addr_int32 : Int32.t = Unix.Inet_addr.inet4_addr_to_int32_exn addr in
  let s = String.create 6 in
  (* TODO why not use binprot *)
  Binary_packing.pack_signed_32 ~byte_order:`Big_endian ~buf:s ~pos:0 addr_int32;
  Binary_packing.pack_signed_16 ~byte_order:`Big_endian ~buf:s ~pos:4 port;
  s

let peer_to_bencode peer_addr = 
  B.String (peer_to_string peer_addr)

let string_to_peer s : Socket.Address.Inet.t =
  let addr_int32 = Binary_packing.unpack_signed_32 ~byte_order:`Big_endian 
      ~buf:s ~pos:0 in
  let port = Binary_packing.unpack_unsigned_16_big_endian ~pos:4 ~buf:s in
  let addr = Unix.Inet_addr.inet4_addr_of_int32 addr_int32 in
  Socket.Address.Inet.create addr port

let bencode_to_peer b : Socket.Address.Inet.t =
  let s = get (B.as_string b) in
  string_to_peer s

let rec bencode_to_peers b =
  let s : string = get (B.as_string b) in
  let peer_addr_length = 6 in
  split s peer_addr_length
  |> Array.map ~f:string_to_peer
  |> Array.to_list

let peers_to_bencode peers = 
  let f acc p = acc ^ (peer_to_string p) in
  B.String (List.fold peers ~init:"" ~f)


let node_to_bencode n = B.String (Node_id.to_string n)

let nodes_to_bencode l = B.String (String.concat (List.map l ~f:Node_id.to_string))

let hashs_to_bencode l = assert false

let hash_to_bencode h = B.String (Bt_hash.to_string h)

let bencode_to_nodes b = 
    let s = get (B.as_string b) in
    Array.map (split s Node_id.length) ~f:Node_id.of_string 
    |> Array.to_list

(* TODO extend module bencode with _exn function and get rid of the get
   elsewhere in the code *)

let get_string_from_dict_exn b s =
  get (B.as_string (get (B.dict_get b s))) 

let get_dict_from_dict_exn b s =
  get (B.dict_get b s)

let get_int_from_dict_exn b s =
  get (B.as_int (get (B.dict_get b s)))

let get_list_from_dict_exn b s =
  get (B.as_list (get (B.dict_get b s)))













