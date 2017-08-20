open Core
open Async
open Bin_prot
open Log.Global

module Kp = Krpc_packet
module Nc = Node_comm

let krpc_timeout = sec 5.0

type t = {
  buffer : Bigstring.t;
  socket : Fd.t;
  id : Node_id.t; (* Id of this node *)
  routing : Routing.t;
  peers : Peers_tbl.t;
  tokens : Tokens.t;
}

let process_query t addr transaction_id q : unit =
  match q with

  | Kp.Ping _ -> 
    let content = Kp.(Response (R_ping_or_get_peers_node t.id)) in 
    Nc.send_packet addr Kp.{ transaction_id; content } 

  | Kp.Find_node (_, n) -> 
    let nis = Routing.find_node t.routing n in
    let content = Kp.(Response  (R_find_node (t.id, nis))) in
    Nc.send_packet addr Kp.{ transaction_id; content } 

  | Kp.Get_peers (n, h) -> 
    (* TODO save token *)
    let token = Bt_hash.random () |> Bt_hash.to_string in
    let resp = 
      match Peers_tbl.find t.peers n with
      | [] -> 
        let n = Bt_hash.to_string h |> Node_id.of_string in
        let nis = Routing.find_node t.routing n in
        Kp.R_get_peers_nodes (t.id, token, nis)
      | addrs -> 
        Kp.R_get_peers_values (t.id, token, addrs)
    in
    let content = Kp.Response resp in
    Nc.send_packet addr Kp.{ transaction_id; content } 

  | Kp.Announce_peer (n, h, p, _) -> 
    (* TODO check token *)
    let addr' = Addr.update_port addr p in
    Peers_tbl.add t.peers n addr';
    let content = Kp.(Response (R_ping_or_get_peers_node t.id)) in 
    Nc.send_packet addr Kp.{ transaction_id; content } 

let extract_query_exn Kp.{ transaction_id; content } =
  match content with 
  | Kp.Query q -> transaction_id, q
  | Kp.Response _ | Kp.Error _ -> failwith "not a query"

let callback t bs buf addr : unit =
  let len = Iobuf.length buf in
  (* TODO: this will do until I figure out this Iobuf thing *)
  let s = Iobuf.to_string buf in
  let bs0 = Bigstring.of_string s in 
  Bigstring.blit ~src:bs0 ~dst:bs ~src_pos:0 ~dst_pos:0 ~len;
  (
    let open Or_error.Let_syntax in
    let%bind m = 
      Or_error.try_with (fun () -> Krpc_packet.bin_read_t len bs ~pos_ref:(ref 0))
    in
    let%map tid, q = 
      Or_error.try_with (fun () -> extract_query_exn m) 
    in
    process_query t addr tid q ) |> ignore

let start t = 
  (info "Node_server: started";
   let bs = Bigstring.create Krpc_packet.buffer_size in
   let stop = never () in
   let config = Udp.Config.create ~stop () in
   Udp.recvfrom_loop ~config t.socket (callback t bs))
  |> don't_wait_for

let create ~port id routing peers tokens = 
  info "Node_server: created on port %d" port;
  let addr = Addr.create Unix.Inet_addr.localhost ~port in
  let%map socket = Udp.bind addr in 
  {
    buffer = Common.create_buf Kp.buffer_size;
    socket = Socket.fd socket; 
    id;
    routing;
    peers;
    tokens;
  }


















