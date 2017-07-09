type t 

val create : int -> t

val length : t -> int

val get : t -> int -> bool

val set : t -> int -> bool -> unit

val is_one : t -> bool

val clear : t -> unit

(** [to_string x] returns a string containing the same sequence of bits
    than [x]. If [length x] isn't a multiple of 8, the last bits of the string
    are set to 0. *)
val to_string : t -> string 

(* copy the bits from the string in the byteset. Ignore bits that don't fit 
   in t *)
val fill_from_string : t -> string -> unit


