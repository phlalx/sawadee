open Core
open Async
open Log.Global

module Em = Error_msg
module G = Global

type t = {
  bitfield_name : string; 
  total_length : int; 
  num_pieces : int; (** number of pieces to be downloaded *)
  downloaded : Bitfield.t;
  requested : Bitfield.t; 
  piece_length : int; (** piece length of all pieces except possibly the last one *)
  pieces : Piece.t Array.t;
  pers : Pers.t;
}

let piece_init pieces_hash piece_length total_len i = 
  let adjusted_piece_length = min (total_len - i * piece_length) piece_length in
  let p = Piece.create i pieces_hash.(i) adjusted_piece_length in
  p

let get_piece t i = t.pieces.(i)

let set_piece_status t i s = 
  assert (not (Bitfield.get t.downloaded i));
  match s with
  | `Requested -> Bitfield.set t.requested i true
  | `Not_requested -> Bitfield.set t.requested i false
  | `Downloaded -> Bitfield.set t.downloaded i true

let is_valid_piece_index t i = i >=0 && i < t.num_pieces

let num_pieces t = t.num_pieces 

let length t = t.total_length

let has_piece t i = Bitfield.get t.downloaded i

let downloaded t = t.downloaded

(* TODO replace with is_empty_bitfield *)
let num_downloaded_pieces t = Bitfield.card t.downloaded

let downloaded_to_string downloaded num_pieces =
  let n = Bitfield.card downloaded in 
  let percent = (100 * n) / num_pieces in
  sprintf "%d/%d pieces (%d%%)" n num_pieces percent

(* TODO ugly... eventually we'll set up a proper structure to 
   be used by module Strategy *)
let not_requested t =
  let rec range n = 
    if n = 0 then [] else (n-1) :: (range (n-1)) 
  in
  let f i = 
    (not (Bitfield.get t.downloaded i)) && (not (Bitfield.get t.requested i))

  in List.filter ~f (range t.num_pieces)

let create info_hash tinfo = 
  let { 
    Torrent.piece_length;
    Torrent.pieces_hash;
    Torrent.files_info; 
    Torrent.total_length;
    Torrent.num_pieces;
    Torrent.num_files;
  } = tinfo in 

  let bitfield_name = sprintf "%s/%s%s"
  (G.torrent_path ()) (Bt_hash.to_hex info_hash)  G.bitset_ext in 

  let%bind pers = Pers.create files_info num_pieces piece_length  in
  let requested = Bitfield.empty num_pieces in
  let f = piece_init pieces_hash piece_length total_length in
  let pieces = Array.init num_pieces ~f  in
  info "Network_file: created network file (%d pieces)" num_pieces;
  let downloaded = 
    try
      In_channel.read_all bitfield_name |> Bitfield.of_string
    with _ -> 
      info "Network_file: can't read bitfield %s. Using empty bitfield" bitfield_name;
      Bitfield.empty num_pieces 
  in
  info "Network_file: read bitfield %s" bitfield_name;
  let stats = downloaded_to_string downloaded num_pieces in 
  (* TODO truncate bitfield *)
  info "Network_file: read %s" stats;

  let read_piece i : unit Deferred.t =
    let p = pieces.(i) in
    Pers.read_piece pers p 
    >>| fun () -> 
    if not (Piece.is_hash_ok p) then
     info "Network_file: can't read piece %d from disk" i
  in

  Deferred.List.iter (Bitfield.to_list downloaded num_pieces) ~f:read_piece
  >>| fun () ->

  let finally () =
    info "Network_file: writing bitfield to file %s" bitfield_name;
    let stats = downloaded_to_string downloaded num_pieces in 
    info "Network_file: written %s" stats;
    (try
       let data = downloaded |> Bitfield.to_string in
       Out_channel.write_all bitfield_name ~data
     with 
       _  ->  info "%s" (Em.can't_open bitfield_name));

    Pers.close_all_files pers 
  in 

  Pers.init_write_pipe pers ~finally |> don't_wait_for;

  {
    total_length;
    num_pieces;
    downloaded;
    requested;
    piece_length;
    pieces;
    pers;
    bitfield_name;
  }

let close t = 
  Pers.close_pipe t.pers |> return

let write_piece t i =
  let p = t.pieces.(i) in 
  Pers.write_piece t.pers p  





