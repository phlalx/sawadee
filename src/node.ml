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

let set_id t id = t.id <- Some id

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

let is_valid_ping_reply tid p =
  let open K in
  match p.content with 
 | Response (R_ping_or_get_peers_node id) when tid = p.transaction_id -> Ok id
 | _ -> Error (Error.of_string "unvalid ping reply")

let ping t = 
  let tid = fresh_tid () in
  let ping_packet = packet tid (K.Query (K.Ping G.node_id)) in
  send_packet t ping_packet 
  >>= fun () ->
  Clock.with_timeout G.krpc_timeout (get_one_packet t)
  >>| function
  | `Timeout -> Error (Error.of_string "timeout")
  | `Result p -> is_valid_ping_reply tid p 













