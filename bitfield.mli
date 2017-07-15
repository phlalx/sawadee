(** This is a bitfield as specified by the peer protocol.

  TODO: make this type private and add conversion to and from
        Int.Set. We can the get read of the bitset datatype 
        entirely. 
 *)

open Core

type t = string
[@@deriving sexp]

(* 
val of_set : Int.Set.t -> t
val to_set : t -> Int.Set.t

val to_string : t -> string
val of_string : string -> len:int -> t
 *)