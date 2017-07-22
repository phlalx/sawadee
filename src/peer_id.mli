
(** 20-byte string *)
type t

val random : unit -> t

val dummy : t

val length : int

val to_string : t -> string

val of_string : string -> t

(* for traces, turn this id into something readables *)
val to_readable_string : t -> string
