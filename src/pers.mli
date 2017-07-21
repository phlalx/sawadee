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
   TODO: improve this and make sure close happen after all writes *)

open Core
open Async

type t

(** [create torrent_name torrent_len files_info num_pieces piece_length ]) 
    open (create if needed) all the files with the expected size. *)
val create : 
  string -> int -> 
  (string * int) list ->
  int ->
  int ->
  t Deferred.t

val close_all_files : t -> unit Deferred.t

val read_bitfield : t -> Bitfield.t Deferred.t

val write_bitfield : t -> Bitfield.t -> unit Deferred.t

val write_piece : t -> Piece.t -> unit

val read_piece : t -> Piece.t -> unit Deferred.t



