
include (module type of Bencode)

val as_string_exn : t -> string

val as_int_exn : t -> int

val as_list_exn : t -> t list

val as_dict_exn : t -> (string * t) list

val dict_get_exn : t -> string -> t 

val get_string_from_dict_exn : t -> string -> string

val get_int_from_dict_exn : t -> string -> int

val get_list_from_dict_exn : t -> string -> t list