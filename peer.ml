open Core
open Async
open Log.Global

type t = {
  mutable choked : bool;
  mutable interested : bool;
  peer : Socket.Address.Inet.t;
  file : File.t
}

let handshake = "\019BitTorrent protocol"

type message_type =
  | Choke
  | Unchoke
  | Interested
  | Not_interested
  | Have
  | Bitfield
  | Request
  | Piece
  | Cancel

type msg = {
  code : message_type;
}

let msg_to_string (m:msg) = 
  match m.code with
  | Choke -> 0
  | Unchoke -> 1
  | Interested -> 2
  | Not_interested -> 3
  | Have -> 4
  | Bitfield -> 5
  | Request -> 6
  | Piece -> 7
  | Cancel -> 8

let create peer file = { peer; interested = false; choked = true; file }

let start st =
  debug "start";
  let wtc = Tcp.to_inet_address st.peer in
  debug "Trying to connect to %s" (Socket.Address.Inet.to_string st.peer);
  Tcp.connect wtc >>= 
  fun (_, r, w) ->
  debug "write";
  Writer.write w handshake;
  let buf = "-----" in
  Reader.read r buf >>= fun s -> 
  debug "%s" buf;
  return ()
