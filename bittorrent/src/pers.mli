(* Persistence. Read and write pieces to files.

   [t] contains a mapping between pieces and segments of the file.

   Network file :  | - piece 1 - | - piece 2 - | ......... | - piece n - |
                   | segment |       segment         | seg | seg |  seg  |

   When created, all files are files are opened to the right size. Write are 
   channeled through a pipe to be processed sequencially.

  TODO: Make this module more generic.
  - Make segment type public 
  - Keep mapping from piece to list of segments in Network_file.t
  - have a function to write a bigstring to list of segments *)

open Core
open Async

type t

val create : 
  Torrent.file_info list ->
  int (* num_pieces *) ->
  int (* piece length *) ->
  t Deferred.t

val close_all_files : t -> unit Deferred.t

val write_piece : t -> Piece.t -> unit

val read_piece : t -> Piece.t -> unit Deferred.t

val init_write_pipe : t -> finally:(unit -> unit Deferred.t) -> unit Deferred.t

val close_pipe : t -> unit

