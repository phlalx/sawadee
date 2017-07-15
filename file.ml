open Core
open Async
open Log.Global

type t = {
  len : int;
  name : string;
  num_pieces : int; (** number of pieces to be downloaded *)
  pieces : Piece.t Array.t;
  info_hash : Bt_hash.t;  (** hash of the info section of the bittorrent file *)
  bitset : Bitset.t; (** TODO change this to a Int.Set *)
  file_fd : Unix.Fd.t;
  bitset_fd : Unix.Fd.t;
  piece_length : int;
}

let bitset_name name = "." ^ name

let create ~len hash pieces_hash ~name ~piece_length =
  let num_pieces = Array.length pieces_hash in
  assert (num_pieces = (len + piece_length - 1) / piece_length);
  let piece_init i = 
    let adjusted_piece_length = min (len - i * piece_length) piece_length in
    Piece.create i pieces_hash.(i) adjusted_piece_length in  
  let pieces = Array.init num_pieces ~f:piece_init  in
  let bitset = Bitset.create num_pieces in
  Unix.openfile name ~mode:[`Creat;`Rdwr]  
  >>= fun file_fd ->
  Unix.openfile (bitset_name name) ~mode:[`Creat;`Rdwr]
  >>| fun bitset_fd ->
  info "create file (num piece = %d, name = %s)" num_pieces name;
  { len; name; num_pieces; pieces; info_hash = hash; bitset; file_fd; bitset_fd; 
    piece_length } 

let bitfield t = Bitset.to_string t.bitset 

let hash t = t.info_hash

let num_piece_have t = Bitset.num_bit_set t.bitset  

let num_pieces t = t.num_pieces 

let get_piece t i = t.pieces.(i)

let set_piece_have t i = Bitset.set t.bitset i true

(* TODO not efficient... *)
let pieces_not_requested t = 
  let f p = Piece.get_status p = `Not_requested in
  let a = Array.filter t.pieces ~f in
  let l = Array.map a ~f:Piece.get_index in
  Int.Set.of_array l

(* TODO something not right...
   should we somehow wait for the write to be completed before
   using seek again?
   For some reason, the file becomes way bigger than what it should be. 
   READ THIS async_unix/Async_unix/Fd.mod/index.html *)
let write_to_disk t =
(*   let f acc p = acc + (Piece.length p) in
  let sum_piece = Array.fold t.pieces ~init:0 ~f  in
  assert(sum_piece = t.len);
 *)  
 Async_unix.Unix_syscalls.lseek t.bitset_fd ~mode:`Set 0L
  >>| fun _ ->
  let wr_bitset = Writer.create t.bitset_fd in
  let wr_file = Writer.create t.file_fd in
  Writer.write wr_bitset (Bitset.to_string t.bitset);
  let f p = 
    if (Piece.get_status p = `Downloaded) then (
      let offset = Piece.file_offset p in
      Async_unix.Unix_syscalls.lseek t.file_fd ~mode:`Set offset
      >>| fun _ ->
      Piece.write p wr_file;
      Piece.set_status p `On_disk)
    else 
      return ()
  in 
  Array.iter t.pieces ~f:(fun p -> don't_wait_for (f p))

let write_to_disk t = 
  don't_wait_for (write_to_disk t)

let close t = 
  Unix.close t.file_fd
  >>= fun () ->
  Unix.close t.bitset_fd

