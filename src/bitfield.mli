(** Bittorrent bitfield. Alias for string. 

    This is used as the serialized
    form of the [Bitset.t], such as defined by the protocol. It is used both
    for messages and for disk storage *) 
open Core

type t
[@@deriving sexp]

val length : t -> int

val of_string : string -> t

val to_string : t -> string
