type t 

(** create an array of bit, initially zeroed  *)
val create : int -> t

val init : int -> f:(int -> bool) -> t

val length : t -> int

val get : t -> int -> bool

val set : t -> int -> bool -> unit

(** returns true if all bits are set to one *)
val is_one : t -> bool

val num_bit_set : t -> int

val clear : t -> unit

val (&) : t -> t -> t

val not : t -> t

val choose : t -> int option

(** [to_string x] returns a string containing the same sequence of bits
    than [x]. If [length x] isn't a multiple of 8, the last bits of the string
    are set to 0. *)
val to_string : t -> string 

(* copy the bits from the string in the byteset. Ignore bits that don't fit 
   in t *)
val fill_from_string : t -> string -> unit

val from_bool_array : bool array -> t

