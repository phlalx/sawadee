open Core
open Async
open Log.Global

module G = Global
module P = Peer_comm
module Em = Error_msg

let ignore_error addr : unit Or_error.t -> unit =
  function 
  | Ok () -> () 
  | Error err -> debug !"Start: can't connect to %{Addr}" addr

let add_peers pwp info_hash addrs : unit Deferred.t =

  let add_peer addr = 
    let open Deferred.Or_error.Monad_infix in 
    P.create_with_connect addr
    >>= fun p ->
    P.initiate_handshake p info_hash 
    >>= fun { extension; dht } -> 
    let peer = Peer.create p ~extension ~dht in 
    Pwp.add_peer pwp peer |> Deferred.ok
    >>= fun () -> 
    Peer.close peer |> Deferred.ok
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
    let%bind addrs = Krpc.lookup info_hash in  (* TODO use a pipe to regulate the number of peers *)
    let num_of_peers = List.length addrs in 
    info "Start: %d DHT peers" num_of_peers;
    add_peers pwp info_hash addrs
  )

let process_any ?uris ?tinfo info_hash : Bt_hash.t =

  let pwp = Pwp.create info_hash in 

  Pwp.start pwp tinfo |> don't_wait_for;

  Option.value_map uris ~default:() ~f:(add_peers_from_tracker pwp info_hash);
  add_peers_from_dht pwp info_hash; 
  Torrent_table.add info_hash pwp; 
  info_hash

let process_magnet hash = 
  let h = Bt_hash.to_hex hash in
  info "Start: processing magnet %s" h;
  let n = h |> G.torrent_name |> G.with_torrent_path in
  match Option.try_with
          (fun () -> In_channel.read_all n |> Torrent.info_of_string) 
  with
  | None -> 
    info "Start: didn't find any meta-info from previous session";
    process_any hash 
  | Some tinfo -> 
    info "Start: found meta-info from previous session";
    process_any hash ~tinfo

let process_string s = 
  let t = try 
      Torrent.of_string s
    with
    | Failure _ -> failwith ("unable to decode" ^ s)
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

