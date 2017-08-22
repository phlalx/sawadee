
type t = Node_id.t * Addr.t  
[@@deriving sexp_of]

val to_compact : t -> string

val of_compact : string -> t

val to_string : t -> string

val of_string_exn : string -> t

val list_to_bencode : t list -> Bencode_ext.t

val list_of_bencode : Bencode_ext.t -> t list

val length : int

val list_to_string : t list -> string

val list_of_string_exn : string -> t list

