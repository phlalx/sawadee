open Core
open Async
open Log.Global

type t = {
  file : File.t;
  peer : Peer.t;
}

let create peer_addr file = 
  Peer.create peer_addr
  >>| function 
  | Ok peer -> Ok { file; peer }
  | Error exn -> Error exn

let init x =
  Peer.handshake x.peer x.file.File.sha 
  >>= fun () ->
  debug "sending keep alive";
  Peer.send_message x.peer Message.KeepAlive
  >>= fun () ->
  debug "waiting for reply";
  Peer.get_message x.peer 
  >>| fun m ->
  debug "got message";
  sexp (Message.sexp_of_t m)