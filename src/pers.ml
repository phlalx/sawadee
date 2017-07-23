
open Core
open Async
open Log.Global
module G = Global

type segment = {
  name : string; (* name for debug only *)
  fd : Unix.Fd.t;
  off_in_file : int;
  off : int; (* offset in network file *)
  len : int
}

let segment_to_string t = sprintf "name = %s off = %d len = %d off_f = %d" 
    t.name t.off t.len t.off_in_file

type t = {
  fds : Unix.Fd.t list;
  segments_of_piece : segment list Array.t;
  piece_length : int;
  wr : Piece.t Pipe.Writer.t;
  rd : Piece.t Pipe.Reader.t;
}

(* TODO: try to simplify/improve this *)
let rec align_along_piece_length l pl =
  match l with 
  | [] -> []
  | {name; fd; len; off; off_in_file } :: t when (off % pl) + len <= pl -> 
    {name; fd; len; off; off_in_file } :: (align_along_piece_length t pl) 
  | {name; fd; len; off; off_in_file } :: t -> 
    let len1 = pl - (off % pl) in
    let len2 = len - len1 in
    assert (len1 > 0);
    assert( ((off + len1) % pl) = 0);
    { name; fd; len = len1; off; off_in_file } :: 
    (align_along_piece_length 
       ({ name; fd; off = off + len1; len = len2; off_in_file = off_in_file + len1 } :: t) 
       pl  )

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
  debug "read piece %d from disk" i;
  let f seg : unit Deferred.t = 
    let { name; fd; off; len; off_in_file } = seg in
    debug "reading piece %d from %s" i (segment_to_string seg);
    let s = Piece.get_bigstring_content p in
    let pos = off % t.piece_length in
    Io.read fd s off_in_file pos len
  in
  Deferred.List.iter t.segments_of_piece.(i) ~f 

let read_from_pipe t =
  let read_piece p = 
    let i = Piece.get_index p in
    debug "read from pipe piece %d" i;
    let f seg : unit Deferred.t = 
      let { name; fd; off; len; off_in_file } = seg in
      debug "writing piece %d to %s" i (segment_to_string seg);
      let s = Piece.get_bigstring_content p in
      let pos = off % t.piece_length in
      Io.write fd s off_in_file pos len 
    in
    Deferred.List.iter t.segments_of_piece.(i) ~f
  in
  Pipe.iter t.rd ~f:read_piece

let open_file name ~len = 
  let pname = (G.path ()) ^ name in
  info "open file %s with length %d" pname len;
  let llen = Int64.of_int len in
  let%bind fd = Unix.openfile pname ~mode:[`Creat;`Rdwr] in
  Unix.ftruncate fd llen >>| fun () ->
  fd

let make_segments fds info_files = 
  let off = ref 0 in  (* TODO can we do it more nicely without ref? *)
  let f (name, len) = let l = name, len, !off in off := !off + len; l in
  let files_with_offset = List.map info_files ~f in
  let f (name, len, off) fd = { name; fd; len; off; off_in_file = 0 } in
  List.map2_exn files_with_offset fds ~f

let create info_files num_pieces piece_length = 
  let f (name, len) = open_file name ~len in
  let%bind fds = Deferred.List.map ~how:`Sequential info_files ~f in
  let rd, wr = Pipe.create () in
  let segments = make_segments fds info_files in
  let segments_of_piece = split_along_piece_length segments piece_length num_pieces in
  let print_list_segments segments = 
    List.iter segments ~f:(fun s -> debug "%s" (segment_to_string s)) in
  let f i ls = 
    debug "piece %d" i;
    print_list_segments ls
  in
  Array.iteri segments_of_piece ~f;
  let t = { fds; segments_of_piece; wr; rd; piece_length; }
  in 
  don't_wait_for (read_from_pipe t);  
  return t

let write_piece t p = 
  debug "piece %d is pushed to the I/O pipe" (Piece.get_index p);
  Pipe.write_without_pushback t.wr p 

let close_all_files t = 
  info "closing all files";
  Deferred.List.iter t.fds ~f:Fd.close

let read_bitfield f ~len = 
  info "reading bitfield from file %s" f;
  let s =
  try 
       In_channel.read_all f 
  with _ -> String.make len '\000'  
  in Bitfield.of_string s

let write_bitfield f bf = 
  info "writing bitfield to file %s" f;
  Out_channel.write_all f ~data:(Bitfield.to_string bf)





