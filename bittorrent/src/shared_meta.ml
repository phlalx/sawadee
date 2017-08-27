open Core
open Async
open Blog

module G = Global

type t = {
  tinfo : Torrent.info;
  tinfo_bin : string; (* TODO use an option *)
  content : Bigstring.t;
  bitfield_name : string; 
  total_length : int; 
  num_pieces : int; (** number of pieces to be downloaded *)
  downloaded : Bitfield.t;
  piece_length : int; (** piece length of all pieces except possibly the last one *)
  pieces : Piece.t Array.t;
  pers : Pers.t;
  mutable requested : int Set.Poly.t;
  mutable seeder : bool;
}

let seeder t = t.seeder

let downloaded_to_string downloaded num_pieces =
  let n = Bitfield.card downloaded in 
  let percent = (100 * n) / num_pieces in
  sprintf "%d/%d pieces (%d%%)" n num_pieces percent

let piece_init bs pos pieces_hash piece_length total_len i = 
  let len = min (total_len - i * piece_length) piece_length in
  Piece.create ~pos ~index:i ~len pieces_hash.(i) bs

let create ~seeder info_hash tinfo = 
  let Torrent.{ 
      name;
      piece_length;
      pieces_hash;
      files_info; 
      total_length;
      num_pieces;
      num_files;
    } = tinfo in 
  info "Shared_meta: create for torrent %s" name;

  let content = Bigstring.create total_length in

  let bitfield_name = 
    Bt_hash.to_hex info_hash |> G.bitset_name |> G.with_torrent_path in

  let%bind pers = Pers.create files_info num_pieces piece_length  in
  let f i = piece_init content (i * piece_length) pieces_hash piece_length total_length i in
  let pieces = Array.init num_pieces ~f in
  let downloaded = 
    try
      match seeder with 
      | false -> In_channel.read_all bitfield_name |> Bitfield.of_string
      | true -> Bitfield.full num_pieces (* TODO check file is there *)
    with _ -> 
      info "Shared_meta: can't read bitfield %s. Using empty bitfield" bitfield_name;
      Bitfield.empty num_pieces 
  in
  info "Shared_meta: read bitfield %s" bitfield_name;
  let stats = downloaded_to_string downloaded num_pieces in 
  (* TODO truncate bitfield *)
  info "Shared_meta: read %s" stats;

  let count = ref 0 in

  let read_piece i : unit Deferred.t =
    incr count;
    let p = pieces.(i) in
    Pers.read_piece pers p 
    >>| fun () -> 
    if not (Piece.is_hash_ok p) then
      info "Shared_meta: can't read piece %d from disk" i
  in

  Deferred.List.iter (Bitfield.to_list downloaded num_pieces) ~f:read_piece
  >>| fun () ->
  info "Shared_meta: read %d pieces from disk" !count;

  let finally () =
    info "Shared_meta: writing bitfield to file %s" bitfield_name;
    let stats = downloaded_to_string downloaded num_pieces in 
    info "Shared_meta: written %s" stats;
    (try
       let data = downloaded |> Bitfield.to_string in
       Out_channel.write_all bitfield_name ~data
     with 
       _  ->  info "can't open %s" bitfield_name);

    Pers.close_all_files pers 
  in 

  Pers.init_write_pipe pers ~finally |> don't_wait_for;
  let tinfo_bin = Torrent.info_to_string tinfo in
  let computed_info_hash = Bt_hash.sha1_of_string tinfo_bin in
  assert (info_hash = computed_info_hash); 
  {
    tinfo_bin;
    tinfo;
    content;
    total_length;
    num_pieces;
    downloaded;
    piece_length;
    pieces;
    pers;
    bitfield_name;
    requested = Set.Poly.empty;
    seeder = (Bitfield.card downloaded) = num_pieces;
  }

let to_string t = t.tinfo.Torrent.name

let meta_length t = String.length (t.tinfo_bin)

let get_piece t i = t.pieces.(i)

let set_downloaded t i = 
  assert (not (Bitfield.get t.downloaded i));
  Bitfield.set t.downloaded i true;
  if (Bitfield.card t.downloaded) = t.num_pieces then
    t.seeder <- true

let is_valid_piece_index t i = i >=0 && i < t.num_pieces

let num_pieces t = t.num_pieces 

let length t = t.total_length

let is_downloaded t i = Bitfield.get t.downloaded i

let downloaded t = t.downloaded

let has_any_piece t = not (Bitfield.is_empty t.downloaded)

let tinfo_bin t = t.tinfo_bin

let tinfo t = t.tinfo

let close t = 
  Pers.close_pipe t.pers |> return

let write_piece t i =
  let p = t.pieces.(i) in 
  Pers.write_piece t.pers p  

let add_requested t i = 
  assert (not (Set.mem t.requested i));
  t.requested <- Set.add t.requested i

let remove_requested t i =
  assert (Set.mem t.requested i);
  t.requested <- Set.remove t.requested i

let requested t = Set.to_list t.requested

let is_requested t i = Set.mem t.requested i






