open Core
open Async
open Log.Global

type t = {
  len : int;
  name : string;
  (* TODO add bitfield length *)
  num_pieces : int; (** number of pieces to be downloaded *)
  pieces : Piece.t Array.t;
  info_hash : Bt_hash.t;  (** hash of the info section of the bittorrent file *)
  owned_pieces : Bitset.t;
  file_fd : Unix.Fd.t;
  bitset_fd : Unix.Fd.t;
  piece_length : int;
}

let num_owned_pieces t = Bitset.card t.owned_pieces

let bitset_name name = "." ^ name

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

let create ~len hash pieces_hash ~name ~piece_length =
  let num_pieces = Array.length pieces_hash in
  assert (num_pieces = (len + piece_length - 1) / piece_length);
  let piece_init i = 
    let adjusted_piece_length = min (len - i * piece_length) piece_length in
    Piece.create i pieces_hash.(i) adjusted_piece_length in  
  let pieces = Array.init num_pieces ~f:piece_init  in
  let owned_pieces = Bitset.empty ~size:num_pieces in
  Unix.openfile name ~mode:[`Creat;`Rdwr]  
  >>= fun file_fd -> (* TODO check these values *)
  Unix.ftruncate file_fd (Int64.of_int len)
  >>= fun _ -> 
  Unix.openfile (bitset_name name) ~mode:[`Creat;`Rdwr]
  >>= fun bitset_fd ->
  Unix.ftruncate bitset_fd (Int64.of_int (Bitset.bitfield_length owned_pieces))
  >>= fun () ->
  info "create file (num piece = %d, name = %s)" num_pieces name;
  let t = { len; name; num_pieces; pieces; info_hash = hash; owned_pieces; 
            file_fd; bitset_fd; piece_length } 
  in 
  read t 
  >>| fun () ->
  t

let bitfield t = Bitset.to_bitfield t.owned_pieces

let hash t = t.info_hash

let num_pieces t = t.num_pieces 

let get_piece t i = t.pieces.(i)

let set_owned_piece t i = Bitset.insert t.owned_pieces i

let pieces_not_requested t = 
  let f i = (Piece.get_status t.pieces.(i)) = `Not_requested in
  Bitset.create ~size:t.num_pieces ~f

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
  Unix.close t.bitset_fd

let has_piece t i = Bitset.belongs t.owned_pieces i