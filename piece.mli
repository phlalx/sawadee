open Core

val block_size : Int32.t

type t = {
  index : int;
  mutable status : [ `Requested | `Downloaded | `Not_requested];
  hash : string;
  length : int;
  content : string;
  blocks : Bitset.t
}

val create : int -> string -> int -> t

(** updates pieces with downloaded block *)
val update : t -> Int32.t -> string -> unit 

(** generate offset / length of missing pieces *)
val blocks : t -> (Int32.t * Int32.t) list
