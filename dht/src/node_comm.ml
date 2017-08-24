
open Core
open Async
open Bin_prot
open Dlog

module Kp = Krpc_packet

type t = {
  addr : Addr.t;
  buffer : Bigstring.t;
  socket : Fd.t;
}

let connect addr = {
  addr;
  buffer = Common.create_buf Kp.buffer_size;
  socket = Socket.create Socket.Type.udp |> Socket.fd;
}

let send_packet addr m : unit =
  let t = connect addr in
  let pos = 0 in 
  let len = Kp.bin_write_t t.buffer ~pos m in 
  let buf = t.buffer |> Iobuf.of_bigstring ~pos ~len in
  let send_or_error = Udp.sendto () |> return in (
  let open Deferred.Or_error.Let_syntax in
  let%bind send = send_or_error in
  Deferred.Or_error.try_with (fun () -> send t.socket buf t.addr))
  |> Deferred.ignore |> don't_wait_for