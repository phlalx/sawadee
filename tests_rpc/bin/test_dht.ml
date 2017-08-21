open Core
open Async
open Log.Global

let piece_length = 65536 

let setup_seeder f port : string Deferred.t =
  match%map Wrapper.seed f port with
  | Ok hash -> Bt_hash.to_hex hash
  | Error err -> Print.printf !"%{sexp:Error.t}\n" err; failwith "can't seed"

let test_leecher uri port : int Deferred.t =
  let%bind handler = Wrapper.add_magnet uri port in
  let%bind () = Clock.after (sec 5.0) in 
  let%bind status = Wrapper.status handler port in 
  let res = 
    match status with
    | Some { torrent = Some { downloaded }} -> Bitfield.card downloaded 
    | _ -> assert false 
  in
  let%map () = Wrapper.terminate port in
  res

let process port num_clients file () : unit Deferred.t = 
  assert (num_clients > 1);
  set_level `Info;
  let l = List.range port (port + num_clients) in 
  match l with
  | hd :: tl -> 
    let%bind hash = setup_seeder (file, piece_length) port in
    let%bind res = Deferred.List.map tl ~f:(test_leecher hash) ~how:`Parallel in
    let s = List.to_string res ~f:string_of_int in
    info "res = %s" s;
    flushed () 
    >>| fun () ->
    List.iter res ~f:(fun x -> assert (x = 66))
  | _ -> assert false

let () = 
  let spec =
    Command.Spec.(
      empty +> 
      flag "-r" (required int) ~doc:"set base port"  +>  
      flag "-n" (required int) ~doc:"set num clients" +>  
      anon ("FILE" %: string))
  in
  let command =
    Command.async ~summary:"Download torrent file" spec process
  in
  Command.run command
