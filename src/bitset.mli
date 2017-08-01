(** Set of integers represented as array of bits. 

    We use set terminology instead of array's since this is how we use this
    module in the program. Most operations are constant time. 

    We refered as [Bitfield.t] the serialized string of the bitset. It is
    used both in messages and on disk to represent the list of owned pieces  
    of the network file. *)
open Core
open Async

(** An element of type [t] represents a set of integer that belongs to \[0, size)
    where [size] is fixed and given at creation *) 
type t 

(** [empty size] is an empty set *)
val empty : size:int -> t

(** [create size f] is the set of int in \[0, size) satisfying predicate [f] *)
val create : size:int -> f:(int -> bool) -> t

(** maximal number of element, don't confuse with [card] *)
val size : t -> int

(** test set membership, raises if outside bounds *)
val belongs : t -> int -> bool

(** insert element, raises if outside bounds *)
val insert : t -> int -> unit

(** test if set equals to [0,size)] *)
val is_full : t -> bool

val is_empty : t -> bool

(** number of elements in set *)
val card : t -> int

(** empty set *)
val reset : t -> unit

(** intersection of sets. Linear in size of set. Raises if different sizes *)
val inter : t -> t -> t

(** complement set. Linear in size *)
val compl : t -> t

(** returns any element if non-empty *)
val choose : t -> int option

(** returns any random element if non-empty *)
val choose_random : t -> int option

val to_list : t -> int list

val to_string : t -> string

(** [to_bitfield x] returns a bitfield as specified by the peer protocol. If 
    [size x] isn't a multiple of 8, the last bits of the string are set to 0. *)
val to_bitfield : t -> Bitfield.t

val of_bitfield : Bitfield.t -> int -> t 

(** [insert_from_bitfield t b] fills t with bits in bitfield [b] as 
    specified by the peer protocol. *)
val insert_from_bitfield : t -> Bitfield.t -> unit


val bitfield_length : t -> int

(** [bitfield_length_from_size n] is size of bitfield from the bitset size.
    This is needed when we need to allocate space for the bitfield. *)
val bitfield_length_from_size : int -> int


