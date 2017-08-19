open Core
open Async
open Log.Global

type t

val create : Block.t Pipe.Writer.t -> Bitfield.t -> t

val start : t -> Network_file.t -> unit

val set_choking : t -> bool -> unit

val close : t -> unit