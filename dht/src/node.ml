open Core
open Async
open Bin_prot
open Log.Global

module Kp = Krpc_packet

let krpc_timeout = sec 5.0

type t = {
  addr : Addr.t;
  buffer : Bigstring.t;
  socket : Fd.t;
  id : Node_id.t; (* Id of querying node *)
}

let connect id addr = {
  addr;
  buffer = Common.create_buf Kp.buffer_size;
  socket = Socket.create Socket.Type.udp |> Socket.fd;
  id;
}

let send_packet t m : unit Deferred.t = 
  let pos = 0 in 
  let len = Kp.bin_write_t t.buffer ~pos m in 
  let buf = t.buffer |> Iobuf.of_bigstring ~pos ~len in
  let send = Udp.sendto () |> Or_error.ok_exn in
  send t.socket buf t.addr

let get_one_packet_or_error t : Krpc_packet.t Deferred.Or_error.t = 
  let bs = Bigstring.create Krpc_packet.buffer_size in
  let ivar = Ivar.create () in
  let stop = Ivar.read ivar |> Deferred.ignore in
  let config = Udp.Config.create ~stop () in

  let callback buf _ : unit =
    let len = Iobuf.length buf in
    (* TODO: this will do until I figure out this Iobuf thing *)
    let s = Iobuf.to_string buf in
    let bs0 = Bigstring.of_string s in 
    Bigstring.blit ~src:bs0 ~dst:bs ~src_pos:0 ~dst_pos:0 ~len;
    let m = (fun () -> Krpc_packet.bin_read_t len bs ~pos_ref:(ref 0))
            |> Or_error.try_with
    in
    Ivar.fill ivar m 
  in
  Udp.recvfrom_loop ~config t.socket callback 
  >>= fun () ->
  Ivar.read ivar

let counter = ref 0 

let fresh_tid () = incr counter; !counter |> string_of_int

let query_packet transaction_id query = 
  Kp.{ transaction_id; content = Query query}

let timeout_or_error 
    (x :[ `Result of ('a Or_error.t) | `Timeout ] Deferred.t) 
  : 'a Deferred.Or_error.t = 
  match%map x with
  | `Timeout -> Error (Error.of_string "timeout")
  | `Result r -> r  

let rpc t 
    (query : Kp.query) 
    (validate_and_extract : Kp.response -> 'a Or_error.t) 
  : 'a Deferred.Or_error.t =
  let open Deferred.Or_error.Monad_infix in
  let tid = fresh_tid () in
  query_packet tid query |> send_packet t |> Deferred.ok
  >>= fun () -> 
  get_one_packet_or_error t |> Clock.with_timeout krpc_timeout |> timeout_or_error 
  >>= function
  | { Kp.transaction_id; Kp.content = Kp.Response r } 
    when tid = transaction_id -> validate_and_extract r |> return 
  | _ -> Error (Error.of_string "RPC error") |> return

let rpc_error = Error (Error.of_string "RPC error: Wrong response")

let ping t = 
  let extract_ping_response = 
    function
    | Kp.R_ping_or_get_peers_node id -> Ok id
    | _ -> rpc_error 
  in 
  let query = Kp.Ping t.id
  in
  rpc t query extract_ping_response

let get_peers t info_hash = 
  let extract_query_response = 
    function
    | Kp.R_get_peers_values (id, token, addrs) -> Ok (`Values addrs)
    | Kp.R_get_peers_nodes (id, token, nodes) -> Ok (`Nodes nodes)
    | _ -> rpc_error
  in 
  let query = Kp.Get_peers (t.id, info_hash)
  in
  rpc t query extract_query_response

let close t = assert false



