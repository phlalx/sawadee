(** Bittorrent bitfield. 

Quick implementation of a bitset using a string. A little annoyance is
that we may need to create a bitfield before knowing its real size. This
happens when we get Have messages before we even know the size of the file
to be downloaded. TODO *)

open Core

type t
[@@deriving sexp, bin_io]

(* length of serialized form of [t] *)
val length : t -> int 

(* number of elements to be stored *)
val empty : int -> t

val of_string : string -> t

val to_string : t -> string

val get : t -> int -> bool

val set : t -> int -> bool -> unit

val copy : src:t -> dst:t -> unit 

val card : t -> int

val is_empty : t -> bool

val to_string_hum : t -> int -> string

(* [to_list t n] only considers the [n] first elements of the bf *)
val to_list : t -> int -> int list 

(* [is_subset n t1 t2] only considers the [n] first elements of the bf *)
val is_subset : int -> t -> t -> bool

