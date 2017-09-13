open Core
open Async
open Blog

module G = Global

(** A better solution for send/receive would be to use
    bin_prot_read and bin_prot_write but unfortunately the current version
    of the library uses a fixed 8-bytes length (4 bytes in bittorrent). *) 

type t = {
  peer_addr : Addr.t;
  reader : Reader.t;
  writer : Writer.t;
  receive_buffer : Bigstring.t;
  send_buffer : Bigstring.t;  
  mutable downloading : bool;
  mutable uploading : bool;
  conn_stat : Conn_stat.t;
} 

type handshake_info = {
  extension : bool;
  dht : bool;
  info_hash : Bt_hash.t;
  peer_id : Peer_id.t
}

let buffer_size = Message.max_size

let create peer_addr r w =
  info !"Peer_comm: create peer_comm  %{Addr}" peer_addr;
  (* this needs to be done so as we don't get errors when remote peers
     closes his connection *)
  Writer.set_raise_when_consumer_leaves w false;
  {
    peer_addr; 
    reader = r; 
    writer = w; 
    (* this should be big enough to contain [Piece.block_size]
       and the message header TODO *)
    receive_buffer = Bin_prot.Common.create_buf 40000;
    send_buffer = Bin_prot.Common.create_buf 40000;
    downloading = false;
    uploading = false;
    conn_stat = Conn_stat.create ();
  }

let to_string t = Addr.to_string t.peer_addr 

(* last bit of sequence set to 1 = DHT support, bit 20 = extension *)

let check_bit s i = 
  let bf = Bitfield.of_string s in 
  Bitfield.get bf i  

let status_bytes = "\000\000\000\000\000\016\000\001"
let dht_bit = 63
let extension_bit = 43

let hs_prefix = "\019BitTorrent protocol" ^ status_bytes

let hs hash pid = hs_prefix ^ hash ^ pid   

let hs_len = (String.length hs_prefix) + Bt_hash.length + Bt_hash.length (* 68 *)

(* handshake:
   initiator sends 20 bytes prefix + info_hash + pid
   respondant sends same sequence with its own pid.
   hash must match.
*)

(** Communication functions *)

(** [handshake x info_hash pid] initiates the pwt protocol with peer [x].

    It consists in a round-trip message of the form.
       fixed_prefix ^ info_hash ^ pid

    The connecting peer is the one initiating the connexion. Both info_hash
    must match. Each peer sends its peer_id, that should match the one returned
    by the tracker (if any... apparently there's none in compact form which
    is the one we're using).  *)


let hs hash pid = sprintf !"%s%{Bt_hash}%{Peer_id}" hs_prefix hash pid

let extract (s : string) : Bt_hash.t * Peer_id.t * bool * bool = 
  let hash_pos = String.length hs_prefix  in
  let peer_pos = hash_pos + Bt_hash.length in
  let prefix = String.sub s  ~pos:20 ~len:8 in
  let dht = check_bit prefix dht_bit in 
  let extension = check_bit prefix extension_bit in 
  let hash = String.sub s ~pos:hash_pos ~len:Bt_hash.length in
  let pid = String.sub s ~pos:peer_pos ~len:Peer_id.length in
  Bt_hash.of_string hash, Peer_id.of_string pid, dht, extension

let rec send_handshake t hash ~initiate : handshake_info Deferred.Or_error.t = 
  let hs = hs hash G.peer_id in
  Writer.write t.writer hs ~len:hs_len;
  match initiate with 
  | `Non_initiator info -> 
    Ok info |> return
  | `Initiator -> 
    let has_hash x = x = hash in 
    receive_handshake t has_hash ~initiate:`Non_initiator

and receive_handshake t (has_hash : Bt_hash.t -> bool) ~initiate : 
  handshake_info Deferred.Or_error.t =
  let buf = String.create hs_len in
  Reader.really_read t.reader buf ~len:hs_len 
  |> Clock.with_timeout G.handshake_timeout
  >>= function
  | `Timeout -> Error (Error.of_string "handshake timeout") |> return
  | `Result `Ok  ->  
    begin
      let hash, pid, dht, extension = extract buf in
      let hi = { info_hash = hash; dht; extension; peer_id = pid } in
      match initiate, has_hash hash with 
      | `Initiator, true ->
        let initiate = `Non_initiator hi in
        send_handshake t hash ~initiate
      | `Non_initiator, true ->
        Ok hi |> return
      | _, false -> Error (Error.of_string "don't have hash") |> return
    end
  | `Result `Eof _ -> Error (Error.of_string "handshake error") |> return

let not_ourselves t hinfo = 
  if hinfo.peer_id = G.peer_id then (
    Error (Error.of_string "trying to handshake with ourselves") |> return
  ) else (
    debug !"Peer_comm %{}: handshake ok" t;
    Ok hinfo |> return
  )

let initiate_handshake t hash = 
  let open Deferred.Or_error.Monad_infix in
  send_handshake t hash ~initiate:`Initiator
  >>= not_ourselves t

let wait_handshake t has_hash = 
  let open Deferred.Or_error.Monad_infix in
  receive_handshake t has_hash ~initiate:`Initiator
  >>= not_ourselves t

let receive t =
  let buf = t.receive_buffer in 
  (* we need to get the prefix length first to know how many bytes to
     read *)
  (* TODO doesn't seem right... I'm not sure this is how bin_prot.read
     is supposed to work. Do we really need to peek the size first  *)
  let prefix_len_substr = Bigsubstring.create buf ~pos:0 ~len:4 in 
  match%bind Reader.really_read_bigsubstring t.reader prefix_len_substr with
  | `Eof _ -> return `Eof
  | `Ok -> (
      let pos_ref = ref 0 in
      let len = Bin_prot.Read.bin_read_network32_int buf ~pos_ref in  
      let msg_substr = Bigsubstring.create buf ~pos:4 ~len in 
      Reader.really_read_bigsubstring t.reader msg_substr
      >>| function
      | `Eof _ -> 
        info !"Peer_comm %{}: closed connection" t;
        `Eof 
      | `Ok -> 
        pos_ref := 0;
        let m = Message.bin_read_t buf ~pos_ref in
        Conn_stat.incr_dl t.conn_stat (Message.payload_size m);
        `Ok m)

let send t m =
  let len = 4 + Message.size m in (* prefix length + message *)
  debug !"Peer_comm %{}: sending %{Message}" t m;
  let buf = t.send_buffer in
  let pos = Message.bin_write_t buf 0 m in
  assert(pos = len); 
  Conn_stat.incr_ul t.conn_stat (Message.payload_size m);
  Writer.write_bigstring t.writer buf ~len

let addr t = Addr.addr t.peer_addr

let create_with_connect addr = 
  let open Deferred.Or_error.Let_syntax in 
  let wtc = Tcp.to_inet_address addr in
  (* debug !"Peer_comm %{Addr}: try connecting" addr; *)
  let%map (_, r, w) = 
    Deferred.Or_error.try_with (function () -> Tcp.connect wtc)
  in
  create addr r w 

let close t = 
  debug !"Peer_comm %{Addr}: closing fd" t.peer_addr;
  Conn_stat.close t.conn_stat;
  Writer.close t.writer
  >>= fun () -> 
  Reader.close t.reader

let downloaded t = Conn_stat.total_dl t.conn_stat

let uploaded t = Conn_stat.total_ul t.conn_stat

let download_speed t = Conn_stat.dl_speed t.conn_stat

let upload_speed t = Conn_stat.ul_speed t.conn_stat

