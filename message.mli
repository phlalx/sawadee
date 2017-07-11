(** Messages in the peer wire protocol. 
    See [https://wiki.theory.org/index.php/BitTorrentSpecification#Message_flow>].

    This module describes the type of messages and conversion to/from binary
    protocol using [Bin_prot]. In binary, messages are of the form
    [<length prefix><message ID><payload>] where [<length prefix>] is the 
    length in bytes of the remaining of the message.     

    TODO: 
    - can we generate conversion functions automatically. *)
open Bin_prot

type t =
  | KeepAlive
  | Choke
  | Unchoke
  | Interested
  | Not_interested
  | Have of int32 (** index *)
  | Bitfield of string (** bitfield *) 
  | Request of int32 * int32  * int32 (** index, begin, length *)
  | Piece of int32 * int32 * string (** index, begin, block *)
  | Cancel of int32 * int32 * int32 (** index, begin, length *)
[@@deriving sexp]

(** size of message *not* including 4-byte {i prefix length}. *)
val size : t -> int

(** read a message, including 4-byte prefix *)
val bin_read_t : t Read.reader

(** write a message, including 4-byte prefix *)
val bin_write_t : t Write.writer

val to_string : t -> string