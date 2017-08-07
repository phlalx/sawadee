open Core
open Async

(** The protocol for communicating between the hello client and server.
    There's a single RPC call exposed, which lets you send and receive a
    string.

    The [bin_query] and [bin_response] arguments are values that contain logic
    for binary serialization of the query and response types, in this case,
    both strings.

    The version number is used when you want to mint new versions of an RPC
    without disturbing older versions.
*)

let add_torrent_rpc = Rpc.Rpc.create
  ~name:"add-torrent"
  ~version:0
  ~bin_query:String.bin_t
  ~bin_response:Bt_hash.bin_t

let add_magnet_rpc = Rpc.Rpc.create
  ~name:"add-magnet"
  ~version:0
  ~bin_query:String.bin_t
  ~bin_response:Bt_hash.bin_t

let terminate_rpc = Rpc.Rpc.create
  ~name:"terminate"
  ~version:0
  ~bin_query:Unit.bin_t
  ~bin_response:Unit.bin_t

let status_rpc = Rpc.Rpc.create
  ~name:"status"
  ~version:0
  ~bin_query:Bt_hash.bin_t
  ~bin_response:Status.bin_t_option