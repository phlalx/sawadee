open Core
open Async
open Log.Global

module G = Global
module Pc = Peer_comm
module Nf = Network_file

let metadata_id = 1

type meta = {
  id : Extension.id;
  total_length : int;
  num_block : int;
  data : string;
  received : bool Array.t
} 

type t = {
  event_wr : Pevent.t Pipe.Writer.t;
  peer : Peer_comm.t;
  mutable meta : meta option;
  nf : Nf.t option
}

let create peer event_wr nf = {
  nf;
  peer;
  event_wr;
  meta = None;
}

let handshake_id = 0 
let metadata_id = 1 

let send_handshake t = 
  let supp_ext = 
    match t.nf with
    | None -> [] 
    | Some nf -> [`Metadata (metadata_id, Nf.meta_length nf)]
  in
  let em = Extension.Handshake supp_ext in
  (* TODO make a function to construct extended messages *)
  Pc.send t.peer (Message.Extended (handshake_id, Extension.to_bin em))

let push_event t e = Pipe.write_without_pushback_if_open t.event_wr e 

let update_data t i s = 
  let { received ; total_length; data } = Option.value_exn t.meta in
  received.(i) <- true;
  let off = i * G.meta_block_size in
  let len = min (total_length - i * G.meta_block_size) G.meta_block_size in
  String.blit ~src:s ~dst:data ~src_pos:0 ~dst_pos:off ~len;
  match Array.fold ~init:true ~f:(fun acc b -> acc && b) received  with
  | true ->
    info "Peer_ext: meta-data complete"; 
    let tinfo = Torrent.info_of_string data in
    Tinfo tinfo |> push_event t
  | false -> ()

let request_meta t = 
  info "Peer_ext: request meta";
  match t.meta with
  | None -> assert false
  | Some {id; total_length; num_block } -> 
    let f i = 
      let em = Extension.Request i in
      let m = Message.Extended (id, Extension.to_bin em) in
      debug !"Peer_ext: sending ext. %{Extension}" em;
      Pc.send t.peer m
    in
    List.range 0 num_block |> List.iter ~f

let process_extended t id em =
  debug !"Peer_ext: message %{Extension}" em; 
  match em with 
  | Extension.Handshake [`Metadata (id, total_length)] ->
    let num_block = (total_length + G.meta_block_size - 1) / 
                    G.meta_block_size in
    let data = String.create total_length in
    let received = Array.create num_block false in
    t.meta <- Some {id; total_length; num_block; data; received};
    push_event t Support_meta
  | Extension.Data (i, s) -> update_data t i s  
  | _ -> debug "Peer_ext: not implemented" 

let close t = Pipe.close t.event_wr