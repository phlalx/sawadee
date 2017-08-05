(* http://www.bittorrent.org/beps/bep_0005.html 

   TODO deal properly with errors. shouldn't raise if data received from nodes
   doesn't have the right format *) 

open Core
open Bin_prot

module B = Bencode_ext

type token = string

exception Krpc_exception of string

type query = 
  | Ping of Node_id.t 
  | Find_node of Node_id.t * Node_id.t  
  | Get_peers of Node_id.t * Bt_hash.t
  | Announce_peer of Node_id.t * Bt_hash.t * int * token 

type response = 
  | R_ping_or_get_peers_node of Node_id.t 
  | R_find_node of Node_info.t
  | R_get_peers_values of Node_id.t * token * Addr.t list 
  | R_get_peers_nodes of Node_id.t * token * Node_info.t list 

type error_code = 
  | Generic_error 
  | Server_error 
  | Protocol_error
  | Method_unknown

type content = 
  | Query of query 
  | Response of response 
  | Error of (error_code * string) (* code and message *)

type t = {
  transaction_id : string;
  content : content; 
  (* version : string (* optional, we don't use it *) *)
}

let error_code_to_string = function
  | Generic_error -> "generic_error"
  | Server_error -> "server_error"
  | Protocol_error -> "protocol_error"
  | Method_unknown -> "method_unknown"

let error_to_string (e, m) =
  (error_code_to_string e) ^ m

let response_to_string = function 
  | R_ping_or_get_peers_node _ -> "r_ping_or_get_peers_node" 
  | R_find_node _ -> "r_find_node"
  | R_get_peers_nodes _ -> "r_get_peers_node"
  | R_get_peers_values _ -> "r_get_peers_values"

let query_to_string = function
  | Ping _ -> "ping"
  | Find_node _ -> "find_node"
  | Get_peers _ -> "get_peers"
  | Announce_peer _ -> "announce_peer"

let content_to_string = function
  | Query q -> query_to_string q
  | Response r -> response_to_string r
  | Error e -> error_to_string e


let to_string { transaction_id; content } = 
  Printf.sprintf "%s %s" transaction_id (content_to_string content)


(* for allocating buffer *)
let buffer_size = 4096  

(******************)

let bencode_of_response =
  function
  | R_ping_or_get_peers_node id -> ("id", Node_id.to_bencode id) :: []
  | R_find_node (id, nodes) -> ("id", Node_id.to_bencode id) :: 
                               ("nodes", (Addr.to_bencode nodes) ) :: []
  | R_get_peers_nodes (id, token, nodes) -> 
    ("id", Node_id.to_bencode id) :: ("token", B.String token) :: 
    ("nodes", (Node_info.list_to_bencode nodes) ) :: []
  | R_get_peers_values (id, token, values) -> 
    ("id", Node_id.to_bencode id) :: ("token", B.String token) ::
    ("values", Addr.list_to_bencode_list values) :: []

let bencode_of_query =
  function
  | Ping id -> 
    let args = B.Dict [ "id", (Node_id.to_bencode id) ]  in
    ("q", B.String "ping") :: ("a", args) :: []
  | Find_node (id, target) -> 
    let args = B.Dict [( "id", Node_id.to_bencode id); 
                       ("target", Node_id.to_bencode target)]  in
    ("q", B.String "find_node") :: ("a", args) :: []
  | Get_peers (id, info_hash) -> 
    let args = B.Dict [( "id", Node_id.to_bencode id); 
                       ("info_hash", Bt_hash.to_bencode info_hash)]  in
    ("q", B.String "get_peers") :: ("a", args) :: [] 
  | Announce_peer (id, info_hash, port, token) -> 
    let args = B.Dict [ ("id", Node_id.to_bencode id); 
                        ("info_hash", Bt_hash.to_bencode info_hash);
                        ("port", B.Integer port);
                        ("token", B.String token) ]  in
    ("q", B.String "announce_peer") :: ("a", args) :: []

let bencode_of_error (code, msg) = 
  let i =
    match code with
    | Generic_error -> 201
    | Server_error -> 202
    | Protocol_error -> 203
    | Method_unknown -> 204
  in B.List [B.Integer i; B.String msg ]

let bencode_of_content =
  function
  | Query q -> ("y", B.String "q") :: (bencode_of_query q)
  | Response r -> ("y", B.String "r") :: ("r", B.Dict(bencode_of_response r)) :: []
  | Error e -> ("y", B.String "e") :: ("e", bencode_of_error e) :: []
let bencode_of_t { transaction_id; content } = 
  let bof = bencode_of_content content in
  B.Dict (("t", B.String transaction_id) :: bof)

(******************)

let query_of_bencode b = 
  let args = B.dict_get_exn b "a" in
  let id = B.dict_get_string_exn args "id" |> Node_id.of_string in
  B.dict_get_string_exn b "q" 
  |> function 
  | "ping" -> Ping id
  | "find_node" ->
    let target = B.dict_get_string_exn args "target" |> Node_id.of_string in
    Find_node (id, target)
  | "get_peers" ->
    let info_hash = B.dict_get_string_exn args "info_hash" |> Bt_hash.of_string in
    Get_peers (id, info_hash)
  | "announce_peer" -> 
    let info_hash = B.dict_get_string_exn args "info_hash" |> Bt_hash.of_string in
    let port = B.dict_get_int_exn args "port" in
    let token = B.dict_get_string_exn args "token" in
    Announce_peer (id, info_hash, port, token)
  | _ -> raise (Krpc_exception (B.pretty_print b))

let response_of_bencode b = 
  let r = B.dict_get_exn b "r" in
  let id = B.dict_get_string_exn r "id" |> Node_id.of_string in
  (B.dict_get r "values", B.dict_get r "token", B.dict_get r "nodes")
  |> function
  | Some v, Some t, _ ->  
    (* according to the specs nodes should be none here, but not always the case *)
    let token = B.as_string_exn t in
    let values = Addr.list_of_bencode_list v in
    R_get_peers_values (id, token, values)
  | None, Some t, Some n -> 
    let token = B.as_string_exn t  in
    let nodes_info = Node_info.list_of_bencode n in
    R_get_peers_nodes (id, token, nodes_info)
  | None, None, Some n -> 
    let nodes = Addr.of_bencode n in
    R_find_node (id, nodes)
  | None, None, None -> 
    R_ping_or_get_peers_node id
  | _ -> raise (Krpc_exception (B.pretty_print b))

let int_to_error_code = function
  | 201 -> Generic_error
  | 202 -> Server_error
  | 203 -> Protocol_error
  | 204 -> Method_unknown
  | _ -> raise (Krpc_exception "unknown error code")

let error_of_bencode_list = 
  function
  | [code; msg] -> 
    let c = B.as_int_exn code in
    let m = B.as_string_exn msg in
    Error (int_to_error_code c,m)
  | _ -> raise (Krpc_exception "wrong error message")

let content_of_bencode b = 
  B.dict_get_string_exn b "y" 
  |> function 
  | "q" -> Query (query_of_bencode b)
  | "r" -> Response (response_of_bencode b)
  | "e" -> B.dict_get_list_exn b "e" |> error_of_bencode_list 
  | _ -> raise (Krpc_exception (B.pretty_print b))

let t_of_bencode b = 
  let transaction_id = B.dict_get_string_exn b "t" in
  let content = content_of_bencode b in
  { transaction_id; content }

(******************)

let bin_read_t len buf ~pos_ref = 
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

let with_dummy_content content = { transaction_id = ""; content } 

let dummy = Query (Ping Node_id.dummy) |> with_dummy_content



