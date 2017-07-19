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

(* disable actual read/write *)
let debug_flag = true 

let read_piece t p = 
  let i = Piece.get_index p in
  info "read piece %d from disk" i;
  let f pf = 
    info "writing piece to %s" (Pfile.to_string pf);
    let s = Piece.get_content2 p in
    Pfile.read pf s (String.length s)
  in
  if debug_flag then
    return ()
  else
    Deferred.List.iter t.piece_to_pfiles.(i) ~f

let read_from_pipe t =
  let read_piece p = 
    let i = Piece.get_index p in
    info "read from pipe piece %d" i;
    let f pf = 
      info "writing piece to %s" (Pfile.to_string pf);
      let s = Piece.get_content2 p in
      Pfile.write pf s (String.length s)
    in
    Deferred.List.iter t.piece_to_pfiles.(i) ~f
  in
  if debug_flag then
    return ()
  else
    Pipe.iter t.rd ~f:read_piece

let read_bitfield t = 
  info "reading bitfield";
  (* let bs = Bigstring.create t.bitfield_len  *)
  let bs = Bigstring.init t.bitfield_len ~f:(fun i -> char_of_int 0)
in
  let f fd = 
    Bigstring.pread_assume_fd_is_nonblocking fd bs ~len:t.bitfield_len ~pos:0 
  in
  match Fd.syscall (* ~nonblocking:true *) t.bitfield_fd f with 
  | `Ok _ ->
      return (Bitfield.of_string (Bigstring.to_string bs))
  | `Already_closed -> assert false
  | `Error exn -> raise exn

let create name len info_files num_pieces piece_length = 
  Unix.openfile name ~mode:[`Creat;`Rdwr; `Nonblock]
  >>= fun bitfield_fd ->
  (* assert (Fd.supports_nonblock bitfield_fd); *)
  info "bitfield name: %s lengh %d" name len; 
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

let close_all_files t = 
  info "closing all files";
  Deferred.List.iter t.pfiles ~f:Pfile.close 


let write_and_close_bitfield t bf = 
  let wr = Writer.create t.bitfield_fd in
  Writer.write wr (Bitfield.to_string bf);
  Writer.close wr

let write_piece t p = 
  debug "Piece %d is pushed to the I/O pipe" (Piece.get_index p);
  Pipe.write_without_pushback t.wr p 
