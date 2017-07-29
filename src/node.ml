open Core
open Async
open Bin_prot
open Log.Global

module K = Krpc_packet

type t = {
  id : Node_id.t;
  status : [`Good | `Bad | `Questionable]; 
  peer_addr : Socket.Address.Inet.t;
  buffer : Bigstring.t;
}

let create peer_addr id = {
  id;
  peer_addr;
  buffer = Common.create_buf K.buffer_size;
  status = `Bad;
}

let get_message t = Deferred.never ()

let send_message t m = 
  let len = K.bin_write_t t.buffer ~pos:0 m in 
  ()

(*
let send_ping addr =
  let ping = { transaction_id = "ab"; content = Query (Ping Global.node_id) } in
  send_message addr ping 
 *)


