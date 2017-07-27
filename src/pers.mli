(* Persistence. Read and write pieces to files.

   [Pers.t] contains file descriptors for all the files (the bitset file
   and all the file to download. It maintains a map from pieces to the 
   [segment] of files used for [read/write_piece].

   A file can be partitionned into several such [segment] sharing the same fd.
   To each [Piece.t] index, we map a list of segment used for reading to/from 
   the disk.

   Network file :  | - piece 1 - | - piece 2 - | ......... | - piece n - |
                   | segment |       segment         | seg | seg |  seg  |

   Files are opened to the right size using [create].

   All pieces write are channeled through a pipe to be processed sequencially 
   TODO: improve this and make sure close happen after all writes

  TODO: Make this module application independent. 
  - Make segment type public 
  - Keep mapping from piece to segments in File.t
  - open/close files in start 
  - process in this module requests of the form write bigstring to list of segments

  PROS: this makes this module self-contained
  CONS: add complexity in File.t and Start *)

open Core
open Async

type t

(** [create files_info num_pieces piece_length] 
    opens (creates if needed) all the files with the expected size. *)

val create : 
  (string * int) list ->
  int (* num_pieces *) ->
  int (* piece length *) ->
  t Deferred.t

val close_all_files : t -> unit Deferred.t

val write_piece : t -> Piece.t -> unit

val read_piece : t -> Piece.t -> unit Deferred.t

val read_bitfield : string -> len:int -> Bitfield.t 

val write_bitfield : string -> Bitfield.t -> unit 

(** process write_piece requests, when close_pipe is closed, 
drain the pipe and execute finally.

TODO: can write_piece request be scheduled after the pipe is closed * *)

val init_write_pipe : t -> finally:(unit -> unit Deferred.t) -> unit Deferred.t

val close_pipe : t -> unit

