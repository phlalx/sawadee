open Core

type t = {
  num_peers : int
} [@@deriving bin_io]

type t_option = t Option.t [@@deriving bin_io]