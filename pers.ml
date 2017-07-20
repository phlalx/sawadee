(** Persistence. 

    Pieces are mapped to [segment] of files.

    A file can be partitionned into several such [segment] sharing the same fd.
    To each [Piece.t] index, we map a list of segment used for reading to/from 
    the disk.

    Network file :  | - piece 1 - | - piece 2 - | ......... | - piece n - |
                    | segment |       segment         | seg | seg |  seg  |

    Files are opened to the right size using [create].

*)
open Core
open Async
open Log.Global
module G = Global

type segment = {
  name : string; (* name for debug only *)
  fd : Unix.Fd.t;
  off : int;
  len : int
}

let segment_to_string t = sprintf "name = %s off = %d" t.name t.off

type t = {
  fds : Unix.Fd.t list;
  bitfield_fd : Unix.Fd.t;
  bitfield_len : int;
  segments_of_piece : segment list Array.t;
  piece_length : int;
  wr : Piece.t Pipe.Writer.t;
  rd : Piece.t Pipe.Reader.t;
}

(* TODO: try to simplify/improve this *)
let rec align_along_piece_size l ps =
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
  let f seg : unit Deferred.t = 
    let { name; fd; off; len } = seg in
    info "reading piece %d from %s" i (segment_to_string seg);
    let s = Piece.get_bigstring_content p in
    let pos = off % t.piece_length in
    Io.read fd s off pos len >>| fun () ->
    assert (Piece.is_hash_ok p)
  in
  Deferred.List.iter t.segments_of_piece.(i) ~f

let read_from_pipe t =
  let read_piece p = 
    let i = Piece.get_index p in
    debug "read from pipe piece %d" i;
    let f seg : unit Deferred.t = 
      let { name; fd; off; len } = seg in
      info "writing piece %d to %s" i (segment_to_string seg);
      let s = Piece.get_bigstring_content p in
      let pos = off % t.piece_length in
      Io.write fd s off pos len 
    in
    Deferred.List.iter t.segments_of_piece.(i) ~f
  in
  Pipe.iter t.rd ~f:read_piece

let open_file name ~len = 
  let pname = G.path ^ name in
  info "create file %s with length %d" pname len;
  let llen = Int64.of_int len in
  let%bind fd = Unix.openfile pname ~mode:[`Creat;`Rdwr] in
  Unix.ftruncate fd llen >>| fun () ->
  fd

let make_segments fds info_files = 
  let off = ref 0 in  (* TODO can we do it more nicely without ref? *)
  let f (name, len) = let l = name, len, !off in off := !off + len; l in
  let files_with_offset = List.map info_files ~f in
  let f (name, len, off) fd = { name; fd; len; off } in
  List.map2_exn files_with_offset fds ~f

let create bf_name bf_len info_files num_pieces piece_length = 
  let%bind bitfield_fd = open_file bf_name bf_len in
  let f (name, len) = open_file name ~len in
  let%bind fds = Deferred.List.map ~how:`Sequential info_files ~f in
  let rd, wr = Pipe.create () in
  let segments = make_segments fds info_files in
  let segments_of_piece = split_along_piece_size segments piece_length num_pieces in
  let t = 
    {
      fds;
      bitfield_fd;
      bitfield_len = bf_len;
      segments_of_piece;
      wr;
      rd;
      piece_length;
    }
  in 
  don't_wait_for (read_from_pipe t);  
  return t

let write_piece t p = 
  debug "Piece %d is pushed to the I/O pipe" (Piece.get_index p);
  Pipe.write_without_pushback t.wr p 

let read_bitfield t = 
  info "reading bitfield";
  let bs = Bigstring.create t.bitfield_len in
  Io.read t.bitfield_fd bs ~off:0 ~pos:0 ~len:t.bitfield_len >>= fun () ->
  return (Bitfield.of_string (Bigstring.to_string bs))

let write_bitfield t bf = 
  let bs = Bigstring.of_string (Bitfield.to_string bf) in
  Io.write t.bitfield_fd bs ~off:0 ~pos:0 ~len:t.bitfield_len 

let close_all_files t = 
  info "closing all files";
  Deferred.List.iter (t.bitfield_fd :: t.fds) ~f:Fd.close
