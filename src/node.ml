open Core

open Async
open Bin_prot
open Log.Global

module K = Krpc_packet
module G = Global

type t = {
  mutable id : Node_id.t option;
  mutable status : [`Good | `Bad | `Questionable]; 
  addr : Socket.Address.Inet.t;
  buffer : Bigstring.t;
  socket : Fd.t;
}

let create addr = 
  {
    id = None;
    addr;
    buffer = Common.create_buf K.buffer_size;
    status = `Questionable;
    socket = Socket.create Socket.Type.udp |> Socket.fd;
  }

let addr t = t.addr

let set_status t s = t.status <- s

let id t = t.id

let to_string t = 
  match t.id with 
  | Some id -> Node_id.to_readable_string id
  | None -> "unknown-id"

let set_id t id = 
  t.id <- Some id

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

let packet transaction_id content = K.{ transaction_id; content }

(* generate template for rpcs *)
let rpc t content validate_and_extract =
  let tid = fresh_tid () in
  packet tid content |> send_packet t  
  >>= fun () ->
  get_one_packet t |> Clock.with_timeout G.krpc_timeout
  >>| function
  | `Timeout -> Error (Error.of_string "timeout")
  | `Result { K.transaction_id; K.content = K.Response r } 
    when tid = transaction_id -> validate_and_extract r
  | _ -> Error (Error.of_string "RPC error") (* could be more specific here *)

let ping t = 
  let open K in
  let validate_ping_reply = 
    function
    | R_ping_or_get_peers_node id -> Ok id
    | _ -> Error (Error.of_string "RPC error: Wrong response")
  in 
  let content = K.Query (K.Ping G.node_id) 
  in
  rpc t content validate_ping_reply 

let get_peers t info_hash = assert false

(*
val get_peers : t -> Bt_hash.t -> [`Values of Socket.Address.Inet.t list 
                                  | `Nodes of Node_id.t list] Deferred.Or_error.t  
 *)




