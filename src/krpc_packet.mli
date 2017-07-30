
open Core
open Async (* only needed for Socket.Address.Inet.t *)
open Bin_prot

module B = Bencode

type token = string

type query = 
  | Ping of Node_id.t 
  | Find_node of Node_id.t * Node_id.t  
  | Get_peers of Node_id.t * Bt_hash.t
  | Announce_peer of Node_id.t * Bt_hash.t * int * token 

type response = 
  | R_ping_or_get_peers_node of Node_id.t 
  | R_find_node of Node_id.t * Socket.Address.Inet.t
  | R_get_peers_values of Node_id.t * token * Socket.Address.Inet.t list 
  | R_get_peers_nodes of Node_id.t * token * 
                         (Node_id.t * Socket.Address.Inet.t) list 

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

val dummy : t

val buffer_size : int

val bin_read_t : int -> t Read.reader

val bin_write_t : t Write.writer

val to_string : t -> string
