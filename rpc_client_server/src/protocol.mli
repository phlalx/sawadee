open Core
open Async

val add_torrent_rpc : (string, Bt_hash.t) Rpc.Rpc.t

val add_magnet_rpc : (string, Bt_hash.t) Rpc.Rpc.t

val terminate_rpc : (unit, unit) Rpc.Rpc.t

val status_rpc : (Bt_hash.t, Status.t_option) Rpc.Rpc.t 