open Core
open Async
open Log.Global

module G = Global
module P = Peer_comm
module Em = Error_msg

let ignore_error addr : unit Or_error.t -> unit =
  function 
  | Ok () -> () 
  | Error err -> 
    info !"Start: can't connect to %{Addr}" addr;
    debug !"Start: %{Error.to_string_hum}" err

let add_peers pwp info_hash addrs : unit Deferred.t =

  let add_peer addr = 
    let open Deferred.Or_error.Monad_infix in 
    P.create_with_connect addr
    >>= fun p ->
    P.initiate_handshake p info_hash 
    >>= fun { extension; dht } -> 
    let peer = Peer.create p ~extension ~dht in 
    Pwp.add_peer pwp peer 
    >>= fun () -> 
    P.close p |> Deferred.ok
  in

  let f addr = add_peer addr >>| ignore_error addr in
  Deferred.List.iter ~how:`Parallel addrs ~f

let add_peers_from_tracker pwp info_hash uris = 
  don't_wait_for (
    let%bind addrs = Tracker_client.query info_hash uris in
    let num_of_peers = List.length addrs in 
    info "Start: %d tracker peers" num_of_peers;
    add_peers pwp info_hash addrs
  )

let add_peers_from_dht pwp info_hash = 
  don't_wait_for (
    let%bind addrs = Krpc.lookup info_hash in
    add_peers pwp info_hash addrs
  )

let process_any ?uris ?tinfo info_hash : Bt_hash.t =

  let pwp = Pwp.create info_hash in 

  let () = match tinfo with 
    | None -> ()
    | Some tinfo -> Pwp.set_nf pwp tinfo |> don't_wait_for
  in
  Option.value_map uris ~default:() ~f:(add_peers_from_tracker pwp info_hash);
  add_peers_from_dht pwp info_hash; 
  Torrent_table.add info_hash pwp; 
  info_hash

let process_magnet hash = 
  info !"Start: processing magnet %{Bt_hash.to_hex}" hash;
  process_any hash

let process_string s = 
  let t = try 
      Torrent.from_string s
    with
    | Failure s -> assert false (* TODO *)
    | ex -> raise ex
  in 

  let open Torrent in
  let { info_hash; announce; announce_list; tinfo } = t in  (* TODO what happened to torrent name? *)

  let uris = 
    match announce_list with
    | [] -> [ announce ]
    | al -> List.dedup (List.concat al) |> List.permute 
  in

  process_any ~uris ~tinfo info_hash

