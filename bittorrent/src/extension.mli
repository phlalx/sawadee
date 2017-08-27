(** Messages in extension protocol.

    http://bittorrent.org/beps/bep_0010.html
    http://bittorrent.org/beps/bep_0009.html
    
   Only the meta-data extension is supported. Most peers send the meta-data
   size in the handshake message. To simplify, we only deal with this case
   and always group these two values. 
   
   TODO: can we use the Message.t reception buffer instead of going through a 
   a string to decode the extension message?

   TODO : rename -> Ext_message *)
    
open Core
open Async

(* extended message ID. 
   0 = handshake, 
   >0 = extended message as specified by the handshake. *)
type id = int 
[@@deriving sexp]

type bin = string
[@@deriving sexp]

type ext = [ `Metadata of id * int ]
[@@deriving sexp]

type t = 
  | Reject of int  
  | Request of int 
  | Data of int * string sexp_opaque
  | Handshake of ext list
  | Unknown
[@@deriving sexp]

val to_bin : t -> bin

val of_bin : bin -> t

val to_string : t -> string