(* http://www.bittorrent.org/beps/bep_0005.html 

   TODO deal properly with errors. shouldn't raise if data received from nodes
   doesn't have the right format *) 

open Core
open Dlog
open Bin_prot

module Be = Bencode_ext

exception Krpc_exception of string

type token = string
[@@deriving sexp_of]
type port = int
[@@deriving sexp_of]

type query = 
  | Ping of Node_id.t 
  | Find_node of Node_id.t * Node_id.t  
  | Get_peers of Node_id.t * Bt_hash.t
  | Announce_peer of Node_id.t * Bt_hash.t * port * token 
  (* TODO add unknown *)
[@@deriving sexp_of]

let query_id = function
  | Ping n -> n
  | Find_node (n, _) -> n
  | Get_peers (n, _) -> n
  | Announce_peer (n,_,_,_) -> n

type response = 
  | R_ping_or_get_peers_node of Node_id.t  (* TODO change the name of this 
                                              constructor, it can be a response for announce too *)
  | R_find_node of Node_id.t * Node_info.t list
  | R_get_peers_values of Node_id.t * token * Addr.t list 
  | R_get_peers_nodes of Node_id.t * token * Node_info.t list 
[@@deriving sexp_of]

type error_code = 
  | Generic_error
  | Server_error
  | Protocol_error 
  | Method_unknown
[@@deriving sexp_of]

type content = 
  | Query of query 
  | Response of response 
  | Error of (error_code * string) 
[@@deriving sexp_of]

type t = {
  transaction_id : string;
  content : content; 
}
[@@deriving sexp_of]

(* for allocating buffer *)
let buffer_size = 4096  

(******************)

let bencode_of_response =
  function
  | R_ping_or_get_peers_node id -> ("id", Node_id.to_bencode id) :: []
  | R_find_node (id, nodes) -> 
    ("id", Node_id.to_bencode id) :: ("nodes", (Node_info.list_to_bencode nodes))
    :: []
  | R_get_peers_nodes (id, token, nodes) -> 
    ("id", Node_id.to_bencode id) :: ("token", Be.String token) :: 
    ("nodes", (Node_info.list_to_bencode nodes) ) :: []
  | R_get_peers_values (id, token, values) -> 
    ("id", Node_id.to_bencode id) :: ("token", Be.String token) ::
    ("values", Addr.list_to_bencode_list values) :: []

let bencode_of_query =
  function
  | Ping id -> 
    let args = Be.Dict [ "id", (Node_id.to_bencode id) ]  in
    ("q", Be.String "ping") :: ("a", args) :: []
  | Find_node (id, target) -> 
    let args = Be.Dict [( "id", Node_id.to_bencode id); 
                        ("target", Node_id.to_bencode target)]  in
    ("q", Be.String "find_node") :: ("a", args) :: []
  | Get_peers (id, info_hash) -> 
    let args = Be.Dict [( "id", Node_id.to_bencode id); 
                        ("info_hash", Bt_hash.to_bencode info_hash)]  in
    ("q", Be.String "get_peers") :: ("a", args) :: [] 
  | Announce_peer (id, info_hash, port, token) -> 
    let args = Be.Dict [ ("id", Node_id.to_bencode id); 
                         ("info_hash", Bt_hash.to_bencode info_hash);
                         ("port", Be.Integer port);
                         ("token", Be.String token) ]  in
    ("q", Be.String "announce_peer") :: ("a", args) :: []

let bencode_of_error (code, msg) = 
  let i =
    match code with
    | Generic_error -> 201
    | Server_error -> 202
    | Protocol_error -> 203
    | Method_unknown -> 204
  in Be.List [Be.Integer i; Be.String msg ]

let bencode_of_content =
  function
  | Query q -> ("y", Be.String "q") :: (bencode_of_query q)
  | Response r -> ("y", Be.String "r") :: ("r", Be.Dict(bencode_of_response r)) :: []
  | Error e -> ("y", Be.String "e") :: ("e", bencode_of_error e) :: []
let bencode_of_t { transaction_id; content } = 
  let bof = bencode_of_content content in
  Be.Dict (("t", Be.String transaction_id) :: bof)

(******************)

let query_of_bencode b = 
  let args = Be.dict_get_exn b "a" in
  let id = Be.dict_get_string_exn args "id" |> Node_id.of_string in
  Be.dict_get_string_exn b "q" 
  |> function 
  | "ping" -> Ping id
  | "find_node" ->
    let target = Be.dict_get_string_exn args "target" |> Node_id.of_string in
    Find_node (id, target)
  | "get_peers" ->
    let info_hash = Be.dict_get_string_exn args "info_hash" |> Bt_hash.of_string in
    Get_peers (id, info_hash)
  | "announce_peer" -> 
    let info_hash = Be.dict_get_string_exn args "info_hash" |> Bt_hash.of_string in
    let port = Be.dict_get_int_exn args "port" in
    let token = Be.dict_get_string_exn args "token" in
    Announce_peer (id, info_hash, port, token)
  | _ -> raise (Krpc_exception (Be.pretty_print b))

let response_of_bencode b = 
  let r = Be.dict_get_exn b "r" in
  let id = Be.dict_get_string_exn r "id" |> Node_id.of_string in
  (Be.dict_get r "values", Be.dict_get r "token", Be.dict_get r "nodes")
  |> function
  | Some v, Some t, _ ->  
    (* according to the specs nodes should be none here, but not always the case *)
    let token = Be.as_string_exn t in
    let values = Addr.list_of_bencode_list v in
    R_get_peers_values (id, token, values)
  | None, Some t, Some n -> 
    let token = Be.as_string_exn t  in
    let nodes_info = Node_info.list_of_bencode n in
    R_get_peers_nodes (id, token, nodes_info)
  | None, None, Some n -> 
    let nodes = Node_info.list_of_bencode n in
    R_find_node (id, nodes)
  | None, None, None -> 
    R_ping_or_get_peers_node id
  | _ -> raise (Krpc_exception (Be.pretty_print b))

let int_to_error_code = function
  | 201 -> Generic_error
  | 202 -> Server_error
  | 203 -> Protocol_error
  | 204 -> Method_unknown
  | _ -> raise (Krpc_exception "unknown error code")

let error_of_bencode_list = 
  function
  | [code; msg] -> 
    let c = Be.as_int_exn code in
    let m = Be.as_string_exn msg in
    Error (int_to_error_code c,m)
  | _ -> raise (Krpc_exception "wrong error message")

let content_of_bencode b = 
  Be.dict_get_string_exn b "y" 
  |> function 
  | "q" -> Query (query_of_bencode b)
  | "r" -> Response (response_of_bencode b)
  | "e" -> Be.dict_get_list_exn b "e" |> error_of_bencode_list 
  | _ -> raise (Krpc_exception (Be.pretty_print b))

let t_of_bencode b = 
  try 
    let transaction_id = Be.dict_get_string_exn b "t" in
    let content = content_of_bencode b in
    { transaction_id; content }
  with
  | _ -> failwith (Bencode_ext.pretty_print b)

(******************)

let bin_read_t len buf ~pos_ref = 
  let s = String.create len in
  Common.blit_buf_string buf s ~len;
  Be.decode (`String s)
  |> t_of_bencode 

let bin_write_t (buf:Common.buf) ~(pos:Common.pos) (x:t) = 
  let b = bencode_of_t x in
  let s = Be.encode_to_string b in (* TODO see if we can write to buffer directly *)
  let len = String.length s in
  Common.blit_string_buf s buf ~len;
  len 

let to_string t = t |> sexp_of_t |> Sexp.to_string 

let query_to_string t = t |> sexp_of_query |> Sexp.to_string 

