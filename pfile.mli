(** A [Pfile.t] represents a file on the file system and a segment of the file.

    A file can be partitionned into several such [PFile.t] sharing the same fd.
    Each [Piece.t] will reference a list of pfiles that it'll use for read/write
    to/from the disk.

    Network file :  | - piece 1 - | - piece 2 - | ......... | - piece n - |
                    | pfile |       pfile         | pf | pf |  pf |   pf  |


    pfiles are opened to the right size using [create], exactly one pfile per 
    file specified by the torrent. Then they are split along a piece size into 
    an array. This array will be used when we initialize pieces.

    pfiles are created and pieces are initialized in [File.create]. 

    TODO: rework this... not a good design
*)

open Core
open Async

type t = {
  name : string;
  fd : Unix.Fd.t;
  len : int;
  off : int;
}

(* let pipe_rd, pipe_wr = Pipe.create (); *)

val split_along_piece_size : t list -> ps:int -> num_piece:int -> (t list) Array.t

val create : string -> len:int -> off:int -> t Deferred.t

val close : t -> unit Deferred.t

(** [read t s ps] read from t.fd at position t.off 
    and put result in string at position off % ps *)
val read : t -> string -> ps:int -> unit Deferred.t

(** [write t s ps] write to t.fd at position t.off 
    data from string at position off % ps *)
val write : t -> string -> ps:int -> unit Deferred.t

val to_string : t -> string


