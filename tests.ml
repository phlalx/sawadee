(** 
   Some integration testing.

   Assume echo-server is running on port 8080 (using [netcat -l 8080 -e cat])
   or [Server] module. Call [test ()] from [main.ml].

   TODO: organize this better.
*)

open Core
open Async
open Log.Global

let port = 8080

(** list of messages to test *)
let msg_test = [
  Message.Piece (234255l, 234511l, "dsfjlasjdflasjdfljalsjdflasdjf");
  Message.Have 2344l;
  Message.KeepAlive;
  Message.Choke;
  Message.Unchoke;
  Message.Interested;
  Message.Not_interested;
  Message.Have 2344l;
  Message.Bitfield "adsfasdfdf";
  Message.Request (234255l, 234511l, 12341023l);
  Message.Piece (234255l, 234511l, "dsfjlasjdflasjdfljalsjdflasdjf");
  Message.Cancel (234255l, 234511l, 12341023l);
] assert false

(** check we receive the same message sent to the server after 
    serialization/deserialization *)
let check_message p m1 = 
  debug "sending message m1";
  Peer.send_message p m1
  >>= fun () ->
  debug "done";
  Peer.get_message p
  >>= fun m2 ->
  debug "received message m2";
  sexp (Message.sexp_of_t m1);
  sexp (Message.sexp_of_t m2); 
  assert(m1 = m2);
  return ()

let rec run_in_sequence (f: 'a -> unit Deferred.t) (l : 'a list) 
  : unit Deferred.t =
  match l with 
  | [] -> return ()
  | h :: t -> 
    f h 
    >>= 
    fun () ->
    run_in_sequence f t 

let test () =
  let p_addr = Socket.Address.Inet.create Unix.Inet_addr.localhost port  in
  let dummy_file = File.create ~len:0 ~sha:"" ~pieces:[""] in
  Peer.create p_addr dummy_file 
  >>= function 
  | None -> assert false
  | Some p -> run_in_sequence (fun m -> check_message p m) msg_test

