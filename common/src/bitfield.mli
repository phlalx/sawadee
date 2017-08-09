(** Bittorrent bitfield. Alias for string. 

    This is used as the serialized
    form of the [Bitset.t], such as defined by the protocol. It is used both
    for messages and for disk storage *) 
open Core

type t
[@@deriving sexp]

(* length of string *)
val length : t -> int 

(* number of elements to be stored *)
val empty : int -> t

val of_string : string -> t

val to_string : t -> string

val get : t -> int -> bool

val set : t -> int -> bool -> unit

val copy : src:t -> dst:t -> unit 

val card : t -> int

val to_string_hum : t -> int -> string

val to_list : t -> int -> int list 