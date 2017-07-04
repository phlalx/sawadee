
open Core
open Async
open Log.Global 

let rec copy_blocks buffer r w =
  Reader.read r buffer
  >>= function
  | `Eof -> return ()
  | `Ok bytes_read ->
    debug "got something!";
    Writer.write w buffer ~len:bytes_read;
    Writer.flushed w
    >>= fun () ->
    copy_blocks buffer r w

let start () = 
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port 8080)
      (fun _addr r w ->
         let buffer = String.create (16 * 1024) in
         copy_blocks buffer r w)
  in
  Deferred.don't_wait_for 
    (Deferred.ignore (host_and_port : 
      (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t))

let _ = 
  start();
  never_returns (Scheduler.go ())