open Core
open Bin_prot

type token = string
type port = int

type query = 
  | Ping of Node_id.t 
  | Find_node of Node_id.t * Node_id.t  
  | Get_peers of Node_id.t * Bt_hash.t
  | Announce_peer of Node_id.t * Bt_hash.t * port * token 
  (* TODO add unknown *)

type response = 
  | R_ping_or_get_peers_node of Node_id.t  (* TODO change the name of this 
    constructor, it can be a response for announce too *)
  | R_find_node of Node_id.t * Node_info.t list
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
  | Error of (error_code * string) 

type t = {
  transaction_id : string;
  content : content; 
}

val buffer_size : int

exception Krpc_exception of string

val bin_read_t : int -> t Read.reader

val bin_write_t : t Write.writer

val to_string : t -> string
