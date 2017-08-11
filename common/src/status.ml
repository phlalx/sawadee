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

type t_option = t option[@@deriving bin_io]

let peer_status_to_string ps = 
  let addr = Unix.Inet_addr.to_string ps.addr in
  sprintf "%-16s %-10s: dl/ul %dB %dB, dl/ul speed %.1fB/s %.1fB/s" 
    addr ps.client ps.dl ps.ul ps.dl_speed ps.ul_speed

let torrent_status_to_string ts =
  sprintf !"%{Torrent.info_to_string_hum}downloaded = %d\n" ts.tinfo 
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