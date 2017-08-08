
include (module type of Bencode)

val as_string_exn : t -> string

val as_int_exn : t -> int

val as_list_exn : t -> t list

val as_dict_exn : t -> (string * t) list

val dict_get_exn : t -> string -> t 

val dict_get_string_exn : t -> string -> string

val dict_get_int_exn : t -> string -> int

val dict_get_list_exn : t -> string -> t list

val split : t -> int -> t list

(* decode first part of the string, and returns remainder.
   a little bit ad-hoc... *)
val decode_beginning_exn : string -> t * string 