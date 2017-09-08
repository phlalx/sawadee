open Core
open Async
open Log.Global

let test file port  =
  let%bind hash = 
    let f = In_channel.read_all file in
    Wrapper.add_torrent f port in
  let%bind () = Clock.after (sec 1.0) in 
  let%bind status = Wrapper.status hash port in 
  let res = (Option.value_exn status).Status.num_downloaded_pieces in
  let%map () = Wrapper.terminate port in
  res

let process port num_clients file : unit Deferred.t = 
  set_level `Info;
  let l = List.range port (port + num_clients) in 
  let%bind res = Deferred.List.map l ~f:(test file) ~how:`Parallel in
  let s = List.to_string res ~f:string_of_int in
  info "res = %s" s;
  flushed () 
  >>| fun () ->
  List.iter res ~f:(fun x -> assert (x = 66))


let () = 
  let spec =
    let open Command.Spec in
    empty +> 
    flag "-r" (required int) ~doc:"set base port"  +>  
    flag "-n" (required int) ~doc:"set num clients" +>  
    anon ("URI/FILE" %: string) 
  in
  let command =
    Command.async ~summary:"Download torrent file" spec
      (fun port num_clients file () -> 
         process port num_clients file)
  in
  Command.run command
