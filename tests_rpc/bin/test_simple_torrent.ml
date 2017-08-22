open Core
open Async
open Log.Global

(* TODO make this a ounit test *)

let test_peer file time port : bool Deferred.t =
  let%bind handler = Wrapper.add_torrent file port in
  let%bind () = Clock.after (sec (float_of_int time)) in 
  let%bind status = Wrapper.status handler port in 
  match status with
  | Some { torrent = Some { tinfo; downloaded }} -> 
    let num_dl = Bitfield.card downloaded in
    let num_total = tinfo.Torrent.num_pieces in
    Print.printf "Peer %d: %d/%d\n" port  num_dl num_total;
    Wrapper.terminate port 
    >>| fun () ->
    num_dl = num_total
  | _ -> 
    Print.printf "Peer %d: didn't get status\n" port;
    Wrapper.terminate port 
    >>| fun () -> 
    false

let process port num_clients time filename () : unit Deferred.t = 
  set_level `Info;
  let file = In_channel.read_all filename in
  let r = List.range port (port + num_clients) in
  let%bind l = Deferred.List.map r ~f:(test_peer file time) ~how:`Parallel in
  match List.fold l ~init:true ~f:(fun acc b -> acc && b) with
  | true -> Print.printf "Test OK\n"; exit 0
  | false -> Print.printf "Test Not OK\n"; exit 1

let () = 
  let spec =
    Command.Spec.(
      empty +> 
      flag "-r" (required int) ~doc:"set base port"  +>  
      flag "-n" (required int) ~doc:"set num clients" +>  
      flag "-s" (required int) ~doc:"time for test to complete" +>  
      anon ("URI/FILE" %: string))
  in
  Command.async ~summary:"Download torrent file" spec process 
  |> Command.run 
