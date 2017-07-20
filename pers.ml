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

let rec align_along_piece_size l ps =
  let open Pfile in
  match l with 
  | [] -> []
  | {name; fd; len; off} :: t when (off % ps) + len <= ps -> 
    {name; fd; len; off} :: (align_along_piece_size t ps) 
  | {name; fd; len; off} :: t -> 
    let len' = ps - (off % ps) in
    assert (len' > 0);
    assert( ((off + len') % ps) = 0);
    { name; fd; len = len'; off } :: 
    (align_along_piece_size 
       ({ name; fd; off = off + len'; len = len - len'} :: t) 
       ps)

(* TODO: try to simplify/improve this *)
let rec split_along_piece_size l ~ps ~num_piece =
  let open Pfile in
  let a = List.to_array (align_along_piece_size l ps) in
  let res = Array.create num_piece [] in 
  let j = ref 0 in
  let cur_len = ref 0 in
  let tl = ref [] in
  let m = Array.length a in
  for i = 0 to num_piece -1 do 
    tl := [];
    cur_len := 0;
    while !j < m && !cur_len < ps do 
      tl := !tl @ [ a.(!j) ];
      cur_len := !cur_len + a.(!j).len;
      incr j;
    done;
    res.(i) <- !tl;
  done;
  assert (!j = m);
  res 

let read_piece t p = 
  let i = Piece.get_index p in
  info "read piece %d from disk" i;
  let f pf = 
    info "reading piece %d from %s" i (Pfile.to_string pf);
    let s = Piece.get_bigstring_content p in
    Pfile.read pf s (Bigstring.length s) >>| fun () ->
    assert (Piece.is_hash_ok p)
  in
  Deferred.List.iter t.piece_to_pfiles.(i) ~f

let read_from_pipe t =
  let read_piece p = 
    let i = Piece.get_index p in
    debug "read from pipe piece %d" i;
    let f pf = 
      info "writing piece %d to %s" i (Pfile.to_string pf);
      let s = Piece.get_bigstring_content p in
      Pfile.write pf s (Bigstring.length s)
    in
    Deferred.List.iter t.piece_to_pfiles.(i) ~f
  in
  Pipe.iter t.rd ~f:read_piece

(* TODO use a more conventional method for reading/write bitfield *)
let read_bitfield t = 
  info "reading bitfield";
  let bs = Bigstring.create t.bitfield_len 
in
  let%bind res_off = Unix.lseek t.bitfield_fd 0L ~mode:`Set in
  assert (res_off = 0L);
  let f fd = 
    Bigstring.read fd bs ~len:t.bitfield_len ~pos:0 
  in
  match Fd.syscall t.bitfield_fd f with 
  | `Ok _ -> return (Bitfield.of_string (Bigstring.to_string bs))
  | `Already_closed -> assert false
  | `Error exn -> raise exn

let create name len info_files num_pieces piece_length = 
  let%bind bitfield_fd = Unix.openfile name ~mode:[`Creat;`Rdwr; `Nonblock] in
  info "bitfield name: %s lengh %d" name len; 
  let llen = Int64.of_int len in
  Unix.ftruncate bitfield_fd llen >>= fun () ->
  let off = ref 0 in  (* TODO ugly *)
  let f (name, len) = let l = (name, len, !off) in off := !off + len; l in
  let files_with_offset = List.map info_files ~f in

  let f (name, len, off) = Pfile.create name ~len ~off in
  Deferred.List.map ~how:`Sequential files_with_offset ~f
  >>| fun pfiles -> 
  let piece_to_pfiles = split_along_piece_size pfiles piece_length num_pieces in
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
  let bs = Bigstring.of_string (Bitfield.to_string bf) in
  let%bind res_off = Unix.lseek t.bitfield_fd 0L ~mode:`Set in
  assert (res_off = 0L);
  let f fd = 
    Bigstring.write fd bs ~len:t.bitfield_len ~pos:0 
  in
  match Fd.syscall t.bitfield_fd f with 
  | `Ok _ ->
      Fd.close t.bitfield_fd
  | `Already_closed -> assert false
  | `Error exn -> raise exn

let write_piece t p = 
  debug "Piece %d is pushed to the I/O pipe" (Piece.get_index p);
  Pipe.write_without_pushback t.wr p 
