(** Messages in the peer protocol. 

    http://www.bittorrent.org/beps/bep_0003.html   Main protocol
    http://www.bittorrent.org/beps/bep_0005.html   DHT extension 
    http://www.bittorrent.org/beps/bep_0010.html   Extension protocol

    This module describes the type of messages and conversion to/from binary
    protocol using [Bin_prot]. In binary, messages are of the form
    [<length prefix><message ID><payload>] where [<length prefix>] is the 
    length in bytes of the remaining of the message.     

    We use OCaml ints instead of the [Int32.t] specified by the protocol. This
    shouldn't be a limitation. *)
    
open Core
open Bin_prot

type t =
  | KeepAlive
  | Choke
  | Unchoke
  | Interested
  | Not_interested
  | Have of int (** index *)
  | Bitfield of Bitfield.t
  | Request of Block.t
  | Block of int * int * string (** index, begin, content *)
  | Cancel of Block.t
  | Port of int 
  | Extended of Extension.id * Extension.bin sexp_opaque
[@@deriving sexp]

val max_size : int

val payload_size : t -> int

(* TODO try to be coherent with how this is done in Core *)
(** size of message *not* including 4-byte {i prefix length}. *)
val size : t -> int

(** read a message, including 4-byte length prefix *)
val bin_read_t : t Read.reader

(** write a message, including 4-byte length prefix *)
val bin_write_t : t Write.writer

val to_string : t -> string



