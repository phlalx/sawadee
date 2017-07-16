open Core
open Async

type t = {
  name : string;
  fd : Unix.Fd.t;
  offset : int64; (* offset in the "big file" *)
  len : int64;
}

let create = assert false

let read_to_pieces = assert false

let write_from_pieces = assert false

let close = assert false

let read_to_string = assert false

let write_to_string = assert false

(* 
let read_bitset t =
  let rd_bitset = Reader.create t.bitset_fd in
  let s = String.create (Bitset.bitfield_length t.owned_pieces) in
  Reader.read rd_bitset s
  >>| fun _ ->  (* TODO check this *)
  let bf = Bitfield.of_string s in
  Bitset.insert_from_bitfield t.owned_pieces bf

let read t = 
  read_bitset t
  >>= fun () ->
  let l = Bitset.to_list t.owned_pieces in 
  info "read files from disk %d/%d pieces read" (num_owned_pieces t) t.num_pieces;
  let f p = Piece.read t.pieces.(p) t.file_fd in 
  Deferred.List.iter l ~f 
 *)

(* 
let write_bitset t =
  Async_unix.Unix_syscalls.lseek t.bitset_fd ~mode:`Set 0L 
  >>| fun _ -> (* TODO check for error *)
  let wr_bitset = Writer.create t.bitset_fd in
  let s = Bitfield.to_string (Bitset.to_bitfield t.owned_pieces) in
  Writer.write wr_bitset s

(* TODO rename write_and_close *)
let write t = 
  info "write files to disk %d/%d pieces saved" (num_owned_pieces t) 
    t.num_pieces;
  (* TODO add ~how *)
  Deferred.Array.iter t.pieces ~f:(fun p -> Piece.write p t.file_fd) 
  >>= fun () ->
  write_bitset t 

let close t = 
  write t 
  >>= fun () ->
  Unix.close t.file_fd
  >>= fun () ->
  Unix.close t.bitset_fd *)