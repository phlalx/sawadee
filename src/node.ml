open Core
open Async
open Bin_prot
open Log.Global

module K = Krpc_packet

type t = {
  id : Node_id.t;
  status : [`Good | `Bad | `Questionable]; 
  addr : Socket.Address.Inet.t;
  buffer : Bigstring.t;
}

let create addr id = {
  id;
  addr;
  buffer = Common.create_buf K.buffer_size;
  status = `Bad;
}

let get_message t = Deferred.never ()

(* why this return a deferred when Writer.write does not *)
let send_message t m = 
  let pos = 0 in 
  let len = K.bin_write_t t.buffer ~pos m in 
  let s = Socket.create Socket.Type.udp in
  let buf = t.buffer |> Iobuf.of_bigstring ~pos ~len in
  let send = Udp.sendto () |> Or_error.ok_exn in
  send (Socket.fd s) buf t.addr

let addr t = t.addr

(*
let send_ping addr =
  let ping = { transaction_id = "ab"; content = Query (Ping Global.node_id) } in
  send_message addr ping 
 *)


