open Core

open Async
open Bin_prot
open Log.Global

module K = Krpc_packet
module G = Global

type t = {
  addr : Addr.t;
  buffer : Bigstring.t;
  socket : Fd.t;
}

let connect addr = {
    addr;
    buffer = Common.create_buf K.buffer_size;
    socket = Socket.create Socket.Type.udp |> Socket.fd;
  }

(* why this returns a deferred when Writer.write does not *)
let send_packet t m = 
  let pos = 0 in 
  let len = K.bin_write_t t.buffer ~pos m in 
  let buf = t.buffer |> Iobuf.of_bigstring ~pos ~len in
  let send = Udp.sendto () |> Or_error.ok_exn in
  send t.socket buf t.addr

let get_one_packet t = 
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
    let m = Krpc_packet.bin_read_t len bs ~pos_ref:(ref 0) in
    Ivar.fill ivar m 
  in

  Udp.recvfrom_loop ~config t.socket callback 
  >>= fun () ->
  Ivar.read ivar

let counter = ref 0 

let fresh_tid () = incr counter; !counter |> string_of_int

let query_packet transaction_id query = 
  K.{ transaction_id; content = Query query}

(* generate template for rpcs *)
let rpc t query validate_and_extract =
  let tid = fresh_tid () in
  query_packet tid query |> send_packet t  
  >>= fun () ->
  get_one_packet t |> Clock.with_timeout G.krpc_timeout
  >>| function
  | `Timeout -> Error (Error.of_string "timeout")
  | `Result { K.transaction_id; K.content = K.Response r } 
    when tid = transaction_id -> validate_and_extract r
  | _ -> Error (Error.of_string "RPC error") (* could be more specific here *)

let rpc_error = Error (Error.of_string "RPC error: Wrong response")

let ping t = 
  let open K in
  let extract_ping_response = 
    function
    | R_ping_or_get_peers_node id -> Ok id
    | _ -> rpc_error 
  in 
  let query = K.Ping G.node_id
  in
  rpc t query extract_ping_response

let get_peers t info_hash = 
  let open K in
  let extract_query_response = 
    function
    | R_get_peers_values (id, token, addrs) -> Ok (`Values addrs)
    | R_get_peers_nodes (id, token, nodes) -> Ok (`Nodes nodes)
    | _ -> rpc_error
  in 
  let query = Get_peers (G.node_id, info_hash)
  in
  rpc t query extract_query_response

let close t = assert false



