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
  sprintf "%-16s %s: dl/ul %dB %dB, dl/ul speed %.2fB/s %.2fB/s." 
  addr ps.client ps.dl ps.ul ps.dl_speed ps.ul_speed

let torrent_status_to_string ts =
  sprintf "downloaded = %d" (Bitfield.card ts.downloaded) 

let to_string t = 
  let p = List.map t.peers ~f:peer_status_to_string |> String.concat ~sep:"\n" in

  match t.torrent with 
  | Some torrent -> sprintf !"%{torrent_status_to_string}\n%s\n" torrent p
  | None -> sprintf "don't know about this torrent\n"