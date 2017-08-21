open Core

type peer_status = {
  dl : int;
  ul : int;
  dl_speed : float;
  ul_speed : float;
  client : string;
  addr : Unix.Inet_addr.t
} [@@deriving bin_io]

type torrent_status = {
  tinfo : Torrent.info;
  downloaded : Bitfield.t
} [@@deriving bin_io]

type t = {
  peers : peer_status list;
  torrent : torrent_status option
} [@@deriving bin_io]

let byte_to_hum = 
  function
  | x when x < 0 -> assert false
  | x when 0 <= x && x < 1024 -> sprintf "%dB" x
  | x when 1024 <= x && x < 1048576  -> sprintf "%dkB" (x / 1024)
  | x -> sprintf "%dMB" (x / 1048576)

let byte_speed_to_hum = 
  function
  | x when x <. 0. -> assert false
  | x when 0. <=. x && x <. 1000. -> sprintf "%.1fB/s" x
  | x when 1000. <=. x && x <. 1000000.  -> sprintf "%.1fkB/s" (x /. 1000.)
  | x -> sprintf "%.1fMB/s" (x /. 1000000.) 

let peer_status_to_string ps = 
  let addr = Unix.Inet_addr.to_string ps.addr in
  sprintf "%-16s %-12s dl/ul %8s %8s, dl/ul speed %10s %10s" 
    addr ps.client (byte_to_hum ps.dl) (byte_to_hum ps.ul) 
    (byte_speed_to_hum ps.dl_speed) (byte_speed_to_hum ps.ul_speed)

let info_to_string_hum i : string =
  let fi =
    match i.Torrent.files_info with
    | [] -> ""
    | _ :: [] -> "" 
    | fis ->
      let s = List.map fis ~f:(fun (n,i) -> sprintf "%s %8s" n (byte_to_hum i))
      in (String.concat ~sep:"\n" s) ^ "\n"
  in

  sprintf "%s\npiece_length: %8s\ntotal_length: %8s\nnum_pieces: %d\n%s" 
    i.name (byte_to_hum i.piece_length) (byte_to_hum i.total_length)
    i.num_pieces fi

let torrent_status_to_string ts =
  sprintf !"%{info_to_string_hum}downloaded = %d\n" ts.tinfo 
    (Bitfield.card ts.downloaded) 

let to_string t = 
  let p = 
    match t.peers with
    | [] -> "waiting for peers"
    | peers -> List.map peers ~f:peer_status_to_string |> String.concat ~sep:"\n" 
  in

  match t.torrent with 
  | Some torrent -> sprintf !"%{torrent_status_to_string}%s\n" torrent p
  | None -> sprintf "waiting to retrieve metainfo\n%s\n" p