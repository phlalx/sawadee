open Core
open Async
open Log.Global

type t = {
  len : int; (** total length of network file *)
  piece_length : int; (** piece length of all pieces except possibly the last one *)
  num_pieces : int; (** number of pieces to be downloaded *)
  pieces : Piece.t Array.t;
  owned_pieces : Bitset.t;
  pfiles : Pfile.t list;
  bitset_pfile : Pfile.t;
}

let retrieve_bitset owned_pieces name len piece_length =
  Pfile.create name ~len ~off:0 
  >>= fun bitset ->
  let s = String.create len in
  Pfile.read bitset s ~ps:piece_length  (* TODO shouldn't be needed *)
  >>| fun () -> 
  let bf = Bitfield.of_string s in
  Bitset.insert_from_bitfield owned_pieces bf;
  bitset

let num_owned_pieces t = Bitset.card t.owned_pieces

(* TODO redo this so that the new file is hidden, starting with a dot 
   Need to take care if name is a path *)
let bitset_name name = (Filename.basename name) ^ "_bitset"

let piece_init pieces_hash pfiles piece_length total_len i = 
  let adjusted_piece_length = min (total_len - i * piece_length) piece_length in
  Piece.create i pieces_hash.(i) adjusted_piece_length pfiles.(i)  

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
  debug "Pfiles after splitting";
  let f pf = debug "Pfile: %s" (Pfile.to_string pf) in
  let g i l = debug "piece %d" i; List.iter l f in  
  Array.iteri pfiles_piece_aligned ~f:g;
  let f = piece_init pieces_hash pfiles_piece_aligned piece_length len in
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
    pfiles;
    bitset_pfile;
  } 

let bitfield t = Bitset.to_bitfield t.owned_pieces

let num_pieces t = t.num_pieces 

let get_piece t i = t.pieces.(i)

let set_owned_piece t i = Bitset.insert t.owned_pieces i

let pieces_not_requested t = 
  let f i = (Piece.get_status t.pieces.(i)) = `Not_requested in
  Bitset.create ~size:t.num_pieces ~f

let has_piece t i = Bitset.belongs t.owned_pieces i

let length t = t.len 

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





