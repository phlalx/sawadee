
(** 20-byte string *)
type t

val random : unit -> t

val dummy : t

val to_string : t -> string

val of_string : string -> t
