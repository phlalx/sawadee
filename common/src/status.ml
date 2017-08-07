open Core

type t = {
  num_peers : int;
  num_downloaded_pieces : int;
} [@@deriving bin_io]

type t_option = t Option.t [@@deriving bin_io]