open Core
open Async
open Log.Global

let host = "localhost"

let wrapper f arg port =
  Common.with_rpc_conn (fun conn -> Rpc.Rpc.dispatch_exn f conn arg)
    ~host ~port

let add_torrent = wrapper Protocol.add_torrent_rpc 

let add_magnet = wrapper Protocol.add_magnet_rpc

let terminate = wrapper Protocol.terminate_rpc () 

let status = wrapper Protocol.status_rpc 

let seed = wrapper Protocol.seed_rpc