open Core
open Async

let base_port = 7000 
let num_clients = 4 
let ports = List.range base_port (base_port + num_clients)
let addrs = List.map ports ~f:(fun port -> Addr.local ~port)

let list_and l = List.fold ~init:true ~f:(fun acc b -> acc && b) l

let create_dhts () =
  let f port = 
    let node_id = Node_id.random () in
    Dht.create ~port node_id ~data_path:"." ~verbose:0 
  in
  List.map ports ~f 

let add_addrs dhts =
  let f dht = 
    List.iter addrs ~f:(fun addr -> Dht.try_add dht addr |> Deferred.ignore |> don't_wait_for)
  in
  List.iter dhts ~f

let rotate l =
  match l with 
  | [] -> []
  | h :: t -> t @ [h]

let add_addrs_ring dhts : unit=
  let rotated_addr = rotate addrs in
  let f dht addr = Dht.try_add dht addr |> Deferred.ignore |> don't_wait_for in
  List.iter2 dhts rotated_addr f |> ignore

let check_ring dhts =
  let f dht =
    let n = List.length (Dht.table dht) in
    let expected = 1 in
    Print.printf !"%{Dht} added %d/%d nodes.\n" dht n expected;
    n = expected
  in
  List.map dhts ~f |> list_and

let check_clique dhts =
  let f dht =
    let n = List.length (Dht.table dht) in
    let expected = num_clients - 1 in
    Print.printf !"%{Dht} added %d/%d nodes.\n" dht n expected;
    n = expected
  in
  List.map dhts ~f |> list_and

let test_clique dhts =
  add_addrs dhts; 
  Clock.after (sec 5.0)
  >>| fun () ->
  check_clique dhts 

let test_ring dhts =
  add_addrs_ring dhts; 
  Clock.after (sec 5.0)
  >>| fun () ->
  check_ring dhts

let test_announce dhts h = 
  let dht = List.hd_exn dhts in
  let port = List.hd_exn ports in
  Dht.announce dht h ~port;
  Clock.after (sec 1.0) 
  >>= fun () ->
  let f dht = 
    let%map addrs = Dht.lookup dht h in 
    Print.printf "received %d addresses\n" (List.length addrs)
  in
  Deferred.List.iter dhts ~f

let test () =
  let dhts = create_dhts () in
  Clock.after (sec 1.0) 
  >>= fun () ->
  test_clique dhts
  >>= fun res ->
  (* assert res; *)
  let h = Bt_hash.random () in
  test_announce dhts h
  >>= fun () ->
  exit 0

let () = 
  test () |> don't_wait_for;
  never_returns (Scheduler.go ())
  
