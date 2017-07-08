open Core
open Async
open Log.Global

module B = Bencode

type torrent_info = {
  name : string;
  info_sha1 : string;
  announce : string;
  piece_length : int;
  pieces : string list;
  length : int;
}

let sha1_length = 20

let num_piece pieces = (String.length pieces) / sha1_length

let get_piece pieces i =
  String.sub pieces ~pos:(i * sha1_length) ~len:sha1_length 

exception Wrong_Format

let get x =
  match x with
  | Some y -> y
  | None -> raise Wrong_Format

let rec split (s:string) pos n acc : string list =
  if pos = n then
    acc
  else
    split s (pos + 20) n ((String.sub s pos 20) :: acc) 

let from_torrent chan =
  let bc = B.decode (`Channel chan) in 
  (* debug "Torrent file bencode decoded = %s" (B.pretty_print bc); *)
  let announce_bc = get (B.dict_get bc "announce") in
  let announce = get (B.as_string announce_bc) in
  let info_dict_bc = get (B.dict_get bc "info") in 
  let info_str = B.encode_to_string info_dict_bc in 
  let pieces_bc = get (B.dict_get info_dict_bc "pieces") in
  let pieces = get (B.as_string pieces_bc) in
  let length_bc = get (B.dict_get info_dict_bc "length") in
  let length = get (B.as_int length_bc) in
  let piece_length_bc = get (B.dict_get info_dict_bc "piece length") in
  let piece_length = get (B.as_int piece_length_bc) in
  let name_bc = get (B.dict_get info_dict_bc "name") in
  let name = get (B.as_string name_bc) in 
  let info_sha1 = Sha1.to_bin (Sha1.string info_str) in
  let pieces = split pieces 0 (String.length pieces) [] in 
  { name; announce; info_sha1; piece_length; pieces; length; }

type tracker_reply = {
  complete : int;
  incomplete : int;
  interval : int;
  peers : Socket.Address.Inet.t list
}

(* in compact representation, each peer is represented by 6 bytes *)
let rec decode_peers s pos acc_peers n =
  if (pos = n) then
    acc_peers
  else (
    let (acc : Int32.t ref) = ref 0l in
    let (port : int ref) = ref 0 in
    for i = pos to pos + 3 do
      let byte = ( 
        match Int32.of_int (int_of_char (String.get s i)) with
        | None -> assert false
        | Some(x) -> x
      ) in
      let open Int32 in
      acc := !acc * 256l;
      acc := !acc + byte
    done;
    for i = pos + 4 to pos + 5 do
      port := !port * 256;
      port := !port + int_of_char (String.get s i)
    done;
    let addr = Unix.Inet_addr.inet4_addr_of_int32 !acc in
    let peer = Socket.Address.Inet.create addr !port in
    (* debug "peer = %s" (Socket.Address.Inet.to_string peer);  *)
    decode_peers s (pos + 6) (peer :: acc_peers) n
  )

let from_tracker_reply s =
  let bc = B.decode (`String s) in 
  (* debug "Tracker reply = %s" (B.pretty_print bc); *)
  let complete = get ((B.as_int (get (B.dict_get bc "complete")))) in
  let incomplete = get ((B.as_int (get (B.dict_get bc "incomplete")))) in
  let interval = get ((B.as_int (get (B.dict_get bc "interval")))) in
  let peers_str = get (B.as_string (get (B.dict_get bc "peers"))) in
  let peers = decode_peers peers_str 0 [] (String.length peers_str) in
  { complete; incomplete; interval; peers; }


