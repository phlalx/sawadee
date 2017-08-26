open Core
open Async

let base_port = 7000 
let num_clients = 10
let ports = List.range base_port (base_port + num_clients)
let addrs = List.map ports ~f:(fun port -> Addr.local ~port)

let create_dhts () =
  let f port = 
    let node_id = Node_id.random () in
    Dht.create ~port node_id ~data_path:"." ~verbose:1 
  in
  List.map ports ~f 

let add_addrs dhts =
  let f dht = 
    List.iter addrs ~f:(fun addr -> Dht.try_add dht addr |> Deferred.ignore |> don't_wait_for)
  in
  List.iter dhts ~f

let check dhts =
  let f dht =
    let n = List.length (Dht.table dht) in
    let expected = num_clients - 1 in
    Print.printf !"%{Dht} added %d/%d nodes.\n" dht n expected;
    assert (n = expected)
  in
  List.iter dhts ~f

let test () =
  let dhts = create_dhts () in
  add_addrs dhts; 
  Clock.after (sec 5.0)
  >>= fun () ->
  check dhts;
  exit 0

let () = 
  test () |> don't_wait_for; 
  never_returns (Scheduler.go ())
  
