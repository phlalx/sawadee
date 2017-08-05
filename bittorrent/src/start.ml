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

let process_magnet hash = 
  info !"Start: processing magnet %{Bt_hash.to_hex}" hash;
  let pwp = Pwp.create () in
  let%map addrs = Krpc.lookup hash in
  add_peers pwp hash addrs;
  hash

let process_torrent t = 
  let open Torrent in
  let { torrent_name; info_hash; announce; announce_list; tinfo } = t in

  (***** get list of peers ****)

  let uris = 
    match announce_list with
    | [] -> [ announce ]
    | al -> List.dedup (List.concat al) |> List.permute 
  in

  let%bind addrs = Tracker_client.query info_hash uris in

  let num_of_peers = List.length addrs in 
  info "Start: %d tracker peers" num_of_peers;

  (******* create pwp and add peers ****)

  (* There are two types of peers. 
     - those that we contact  (got their addresses from the tracker
     - those that contact us (via the server) *)

  let bf_name = sprintf "%s/%s%s" (G.torrent_path ()) (Filename.basename torrent_name) 
      G.bitset_ext in

  let%map nf = Network_file.create tinfo bf_name in

  let pwp = Pwp.create ~nf () in

  add_peers pwp info_hash addrs |> don't_wait_for;

  Torrent_table.add info_hash pwp; 

  info_hash

let process_string s = 
  let t = try 
      Torrent.from_string s
    with
    | Failure s -> assert false (* TODO *)
    | ex -> raise ex
  in 
  process_torrent t

