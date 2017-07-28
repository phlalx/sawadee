(* http://www.bittorrent.org/beps/bep_0005.html *)

open Core
open Async
open Log.Global
open Bin_prot

module B = Bencode

type token = string

type query = 
  | Ping of Node_id.t 
  | Find_node of Node_id.t * Node_id.t  
  | Get_peers of Node_id.t * Bt_hash.t
  | Announce_peer of Node_id.t * Bt_hash.t * int * token 

type response = 
  | R_ping 
  | R_find_node of Socket.Address.Inet.t
  | R_get_peers_values of token * Socket.Address.Inet.t list
  | R_get_peers_nodes of token * Node_id.t list
  | R_announce_peer of Node_id.t list

type content = 
  | Query of query 
  | Response of Node_id.t * response 
  | Error of int * string (* code and message *)

type t = {
  transaction_id : string;
  content : content; 
  (* version : string (* optional, we don't use it *) *)
}

(* for allocating buffer *)
let buffer_size = 4096  

let bencode_of_content =
  function
  | Query (Ping id) -> [ ("y", B.String "q"); ("q", B.String "ping");
                         ("a", B.String (Node_id.to_string id)) ]
  | _ -> assert false

let bencode_of_t { transaction_id; content } = 
  let bof = bencode_of_content content in
  B.Dict (("t", B.String transaction_id) :: bof)

let get_string_from_dict_exn b s =
  let open Bencode_utils in
  get (B.as_string (get (B.dict_get b s))) 

let query_of_bencode b = 
  let q = get_string_from_dict_exn b "q" in
  if q = "ping" then
    let node_id = get_string_from_dict_exn b "a" in
    Query (Ping (Node_id.of_string node_id))
  else 
    assert false

let content_of_bencode b = 
  let y = get_string_from_dict_exn b "y" in
  if y = "q" then
    query_of_bencode b 
  else
    assert false

(* TODO maybe extend module bencode with as_string_exn to get rid of these
   annoying get *)
let t_of_bencode b = 
  let transaction_id = get_string_from_dict_exn b "t" in
  let content = content_of_bencode b in
  { transaction_id; content }

let bin_read_t buf ~pos_ref len = 
  let s = String.create len in
  Common.blit_buf_string buf s ~len;
  let b = B.decode (`String s) in
  t_of_bencode b

let bin_write_t (buf:Common.buf) ~(pos:Common.pos) (x:t) = 
  let b = bencode_of_t x in
  let s = B.encode_to_string b in (* TODO see if we can write to buffer directly *)
  let len = String.length s in
  Common.blit_string_buf s buf ~len;
  len 

let link = ()














