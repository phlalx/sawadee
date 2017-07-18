open Core
open Async
open Log.Global
module G = Global

type t = {
  pfiles : Pfile.t list;
  bitfield_fd : Unix.Fd.t;
  bitfield_len : int;
  piece_to_pfiles : Pfile.t list Array.t
}


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
  {
    pfiles = [];
    bitfield_fd;
    bitfield_len = len;
    piece_to_pfiles;
  }

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



(*)
let create pieces_hash ~torrent_name ~piece_length files =

  let num_pieces = Array.length pieces_hash in
  let len = List.fold files ~init:0 ~f:(fun acc (_,l) -> l + acc) in 

  (* TODO: move that to a validation function of the torrent file *)
  assert (num_pieces = (len + piece_length - 1) / piece_length);

  let owned_pieces = Bitset.empty ~size:num_pieces in
  let bf_len = Bitset.bitfield_length owned_pieces in

  let off = ref 0 in  (* TODO ugly *)
  let f (name, len) = let l = (name, len, !off) in off := !off + len; l in
  let files_with_offset = List.map files ~f in

  let name = bitset_name torrent_name in
  retrieve_bitset owned_pieces name bf_len piece_length  
  >>= fun bitset_pfile ->
  info "Retrieved persistent bitset: %d pieces saved" (Bitset.card owned_pieces);
  let f (name, len, off) = Pfile.create name ~len ~off in
  Deferred.List.map ~how:`Sequential files_with_offset ~f
  >>= fun pfiles -> 
  let pfiles_piece_aligned = Pfile.split_along_piece_size pfiles piece_length num_pieces in
(*   debug "Pfiles after splitting";
  let f pf = debug "Pfile: %s" (Pfile.to_string pf) in
  let g i l = debug "piece %d" i; List.iter l f in  
  Array.iteri pfiles_piece_aligned ~f:g;
 *)  let f = piece_init pieces_hash pfiles_piece_aligned piece_length len in
  let pieces = Array.init num_pieces ~f  in
  let _f i p =
    if Bitset.belongs owned_pieces i then (
      Piece.read p
      >>| fun () ->
      Piece.set_status p `On_disk  
    ) else  (
      return ()
    )
  in
  (* Deferred.Array.iteri ~how:`Sequential pieces ~f *)
  Deferred.unit 
  >>| fun () ->
  info "create file (num piece = %d, name = %s)" num_pieces torrent_name;
  { len; 
    num_pieces; 
    owned_pieces; 
    piece_length;
    pieces;
  } 


let pipe_rd, pipe_wr = Pipe.create ();

  let _f i p =
    if Bitset.belongs owned_pieces i then (
      Piece.read p
      >>| fun () ->
      Piece.set_status p `On_disk  
    ) else  (
      return ()
    )
  in



let write_to_file_and_close t = 
  info "Write downloaded pieces";
  let f p =
    if Piece.get_status p = `Downloaded then ( 
      Piece.write p
      >>| fun () ->
      Piece.set_status p `On_disk
    ) else (
      return ()
    )
  in

  Deferred.Array.iter ~how:`Sequential t.pieces ~f
  >>= fun () ->
  info "Write persistent bitset (%d pieces)" (Bitset.card t.owned_pieces);
  let s = Bitfield.to_string (Bitset.to_bitfield t.owned_pieces) in
  Pfile.write t.bitset_pfile s ~ps:t.piece_length 
  >>= fun _ ->
  Pfile.close t.bitset_pfile
  >>= fun () ->
  Deferred.List.iter ~how:`Sequential t.pfiles ~f:Pfile.close


(** write piece to disk if downloaded, set status to `On_disk *)
val write : t -> unit Deferred.t

(** read piece from disk if on disk, set status to `On_disk *)
val read : t -> unit Deferred.t

(** read piece from disk if on disk, set status to `On_disk *)
val read : t -> unit Deferred.t

(** for logging purpose *)
val to_string : t -> string

let create = assert false

(* TODO redo this so that the new file is hidden, starting with a dot 
   Need to take care if name is a path *)


  let len_pfiles = List.fold pfiles ~init:0 ~f:(fun acc l -> l.Pfile.len + acc) in 
  assert (len_pfiles = len);

let open_pfiles t = assert false



let write_bitfield t s = assert false

let write_piece t i = assert false

let read_piece t i = assert false

let close_pfiles t = assert false



let read t =
  info "read piece %d from disk" t.index;
  let f pf = 
    Pfile.read pf t.content ~ps:t.length in
  Deferred.List.iter t.pfiles ~f

let write t =
  info "write piece %d to disk" t.index;
    assert (Bitset.is_full t.blocks); 
    assert ((String.length t.content) = t.length);
    let f pf =
      Pfile.write pf t.content ~ps:t.length in
    Deferred.List.iter t.pfiles ~f *)