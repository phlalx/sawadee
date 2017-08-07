open Core
open Async
open Log.Global

let host = "localhost"

let wrapper f arg port =
  Common.with_rpc_conn (fun conn -> Rpc.Rpc.dispatch_exn f conn arg)
    ~host ~port

let add_torrent s port = wrapper Protocol.add_torrent_rpc s port 

let add_magnet s port = wrapper Protocol.add_magnet_rpc s port

let terminate port = wrapper Protocol.terminate_rpc () port

let status s port = wrapper Protocol.status_rpc s port