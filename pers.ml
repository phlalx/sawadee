open Core
open Async
open Log.Global
module G = Global

type t = {
  pfiles : Pfile.t list;
  bitfield_fd : Unix.Fd.t;
  bitfield_len : int;
  piece_to_pfiles : Pfile.t list Array.t;
  wr : Piece.t Pipe.Writer.t;
  rd : Piece.t Pipe.Reader.t;
}

let read_from_pipe t =
  let read_piece p = 
    let i = Piece.get_index p in
    info "read from pipe piece %d" i;
    let f pf = 
      info "writing piece to %s" (Pfile.to_string pf);
    in
    List.iter t.piece_to_pfiles.(i) ~f;
    return () 
  in
  Pipe.iter t.rd ~f:read_piece

let create name len info_files num_pieces piece_length = 
  Unix.openfile name ~mode:[`Creat;`Rdwr]
  >>= fun bitfield_fd ->
  let llen = Int64.of_int len in
  Unix.ftruncate bitfield_fd llen
  >>= fun () ->

  let off = ref 0 in  (* TODO ugly *)
  let f (name, len) = let l = (name, len, !off) in off := !off + len; l in
  let files_with_offset = List.map info_files ~f in

  let f (name, len, off) = Pfile.create name ~len ~off in
  Deferred.List.map ~how:`Sequential files_with_offset ~f
  >>| fun pfiles -> 
  let piece_to_pfiles = Pfile.split_along_piece_size pfiles piece_length num_pieces in
  let rd, wr = Pipe.create () in
  let t = 
    {
      pfiles = [];
      bitfield_fd;
      bitfield_len = len;
      piece_to_pfiles;
      wr;
      rd;
    }
  in 
  don't_wait_for (read_from_pipe t);  
  t

let close t = 
  Unix.close t.bitfield_fd

let read_bitfield t = 
  let s = String.create t.bitfield_len in
  let rd = Reader.create t.bitfield_fd in
  Reader.read rd s ~len:t.bitfield_len 
  >>| fun _ ->
  Bitfield.of_string s 

let write_and_close_bitfield t bf = 
  let wr = Writer.create t.bitfield_fd in
  Writer.write wr (Bitfield.to_string bf);
  Writer.close wr

let write_to_pipe t p = 
  info "Piece %d is pushed to the I/O pipe" (Piece.get_index p);
  Pipe.write_without_pushback t.wr p

