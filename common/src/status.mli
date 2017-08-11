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
  torrent : torrent_status Option.t
} [@@deriving bin_io]

type t_option = t Option.t [@@deriving bin_io]

val peer_status_to_string : peer_status -> string

val torrent_status_to_string : torrent_status -> string

val to_string : t -> string