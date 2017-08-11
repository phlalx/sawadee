open Core

type peer_status = {
  dl : int;
  ul : int;
  dl_speed : float;
  ul_speed : float;
  client : string;
  addr : Unix.Inet_addr.t 
}
(* 
type nf_status = {
  tinfo : Torrent.info;
  downloaded : Bitfield.t
}
 *)
type t = {
  num_peers : int;
  num_downloaded_pieces : int;
} [@@deriving bin_io]

type t_option = t Option.t [@@deriving bin_io]

