open Core
open Async
open Blog

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
  info_hash : Bt_hash.t;
  event_wr : Pevent.t Pipe.Writer.t;
  peer : Peer_comm.t;
  mutable meta : meta option;
  nf : Nf.t option (* TODO doesn't need the full nf here *)
}

let create info_hash peer event_wr nf = 
  
  info "Peer extension: created with nf = %b" (Option.is_some nf);
  {
  info_hash;
  nf;
  peer;
  event_wr;
  meta = None;
}

let handshake_id = 0 
let metadata_id = 1 

let send peer id e =
  Pc.send peer (Message.Extended (id, Extension.to_bin e))

let send_handshake t = 
  let supp_ext = 
    match t.nf with
    | None -> []  (* we could send simply a metadata_id, but would it be useful
                     to the recipient without length *)
    | Some nf -> [`Metadata (metadata_id, Nf.meta_length nf)]
  in
  let em = Extension.Handshake supp_ext in
  send t.peer handshake_id em

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
    assert (t.info_hash = Bt_hash.sha1_of_string data);
    let tinfo = Torrent.info_of_string data in
    let data' = Torrent.info_to_string tinfo in 
    assert (data = data');
    Tinfo tinfo |> push_event t
  | false -> ()

let request_meta t = 
  info "Peer_ext: request meta";
  match t.meta with
  | None -> assert false
  | Some {id; total_length; num_block } -> 
    let f i = 
      let em = Extension.Request i in
      debug !"Peer_ext: sending ext. %{Extension}" em;
      send t.peer id em
    in
    List.range 0 num_block |> List.iter ~f

let process_extended t rec_id em =
  debug !"Peer_ext: message %{Extension}" em; 
  match em with 
  | Extension.Handshake [] -> () 
  | Extension.Handshake [`Metadata (id, total_length)] when rec_id = handshake_id ->
    let num_block = (total_length + G.meta_block_size - 1) / 
                    G.meta_block_size in
    let data = String.create total_length in
    let received = Array.create num_block false in
    t.meta <- Some {id; total_length; num_block; data; received};
    push_event t Support_meta
  | Extension.Data (i, s) when rec_id = handshake_id (* TODO checkt this *) -> update_data t i s  
  | Extension.Request i ->  (
      assert (rec_id = metadata_id);
      match t.nf with 
      | None -> send t.peer handshake_id (*TODO*) (Extension.Reject i)
      | Some nf ->
      let pos = i * G.meta_block_size in
      info "pos = %d tinfo_bin = %d len = %d" pos (String.length (Nf.tinfo_bin nf)) G.meta_block_size;
      let tinfo_bin_len = String.length (Nf.tinfo_bin nf) in
      assert (pos < tinfo_bin_len);
      let len = min G.meta_block_size (tinfo_bin_len - pos) in 
      let data = String.sub (Nf.tinfo_bin nf) ~pos ~len in
      send t.peer handshake_id (* TODO *) (Extension.Data (i, data))
    )
  | _ -> debug "Peer_ext: not implemented" 

let close t = Pipe.close t.event_wr