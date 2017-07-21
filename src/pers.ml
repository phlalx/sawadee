
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

let segment_to_string t = sprintf "name = %s off = %d len = %d" t.name t.off t.len

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
let rec align_along_piece_length l pl =
  match l with 
  | [] -> []
  | {name; fd; len; off} :: t when (off % pl) + len <= pl -> 
    {name; fd; len; off} :: (align_along_piece_length t pl) 
  | {name; fd; len; off} :: t -> 
    let len' = pl - (off % pl) in
    assert (len' > 0);
    assert( ((off + len') % pl) = 0);
    { name; fd; len = len'; off } :: 
    (align_along_piece_length 
       ({ name; fd; off = off + len'; len = len - len'} :: t) 
       pl)

(* TODO: try to simplify/improve this *)
let rec split_along_piece_length l pl num_pieces =
  let a = List.to_array (align_along_piece_length l pl) in
  let res = Array.create num_pieces [] in 
  let j = ref 0 in
  let cur_len = ref 0 in
  let tl = ref [] in
  let m = Array.length a in
  for i = 0 to num_pieces -1 do 
    tl := [];
    cur_len := 0;
    while !j < m && !cur_len < pl do 
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
    (* assert (Piece.is_hash_ok p); *)
    info "ok";

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
  let pname = (G.path ()) ^ name in
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
  let segments_of_piece = split_along_piece_length segments piece_length num_pieces in
  let print_list_segments segments = 
    List.iter segments ~f:(fun s -> info "segment %s" (segment_to_string s)) in
  let f i ls = 
    info "piece %d" i;
    print_list_segments ls
  in
  Array.iteri segments_of_piece ~f;
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
