open Core
open Async
open Log.Global

type t = {
  file : File.t;
  peer : Peer.t;
}

let create peer_addr file = 
  Peer.create peer_addr
  >>| fun peer ->
  { file; peer }

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
  

(*
  example of error managment with option type

  let peer_addr = List.hd_exn peer_addrs in 
  Peer.create peer_addr file 
  >>= function 
  | None -> return(debug "can't connect to host")
  | Some p -> 
    Peer.handshake p >>= function
    | None -> return(debug "handshake failed")
    | Some p -> exit 0
    *)
