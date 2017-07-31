open OUnit
open Krpc_packet
open Bin_prot
open Async (* TODO don't use? *)

let id1 = Node_id.of_string "abcdefghijklmonopqrs"
let id2 = Node_id.of_string "abc23fgh4jklmonopqrs"
let id3 = Node_id.of_string "abcdefghajklmogopqrs"
let id4 = Node_id.of_string "abcdefghgjklgogopqrs"

let hash1 = Bt_hash.of_string "abcbebghajklmogbpqrs"
let hash2 = Bt_hash.of_string "abcdefgagaaaaaaapqrs"

let token = "this is a token"

let addr1 = 
  let addr = Unix.Inet_addr.of_string "168.123.13.12" in Addr.create addr 129 
let addr2 = 
  let addr = Unix.Inet_addr.of_string "164.120.13.121" in Addr.create addr 10001 

let ping = Query (Ping id1)

let find_node = Query (Find_node (id2, id3))

let get_peers = Query (Get_peers (id1, hash1))

let announce_peer = Query (Announce_peer (id1, hash1, 1234, token))

let r_ping = Response (R_ping_or_get_peers_node id1)

let r_find_node = Response (R_find_node (id1, addr1))

let r_get_peers_values = Response 
    (R_get_peers_values (id1, token, [addr1; addr2]))

let r_get_peers_nodes = 
  Response (R_get_peers_nodes (id1, token, [id2, addr1; id3, addr2])) 

let error = Error (Protocol_error, "this is an error")

let t content = { transaction_id = "abc"; content } 

let idem t = 
  let buf = Common.create_buf buffer_size in 
  let len = bin_write_t buf ~pos:0 t in
  let pos_ref = ref 0 in
  bin_read_t len buf ~pos_ref  

let test1 test_ctxt = 
  let x = t ping in
  assert_equal x (idem x)

let test2 test_ctxt = 
  let x = t find_node in
  assert_equal x (idem x)

let test3 test_ctxt = 
  let x = t get_peers in
  assert_equal x (idem x)

let test4 test_ctxt = 
  let x = t announce_peer in
  assert_equal x (idem x)

let test5 test_ctxt = 
  let x = t r_ping in
  assert_equal x (idem x)

let test6 test_ctxt = 
  let x = t r_find_node in
  assert_equal x (idem x)

let test7 test_ctxt = 
  let x = t r_get_peers_values in
  assert_equal x (idem x)

let test8 test_ctxt = 
  let x = t r_get_peers_nodes in
  assert_equal x (idem x)

let test9 test_ctxt = 
  let x = t error in
  assert_equal x (idem x)

let suite =
  "suite" >::: 
  [ 
    "test1" >:: test1;
    "test2" >:: test2;
    "test3" >:: test3;
    "test4" >:: test4;
    "test5" >:: test5;
    "test6" >:: test6;
    "test7" >:: test7;
    "test8" >:: test8;
    "test9" >:: test8;
  ]

let suite2 =
  "suite2" >::: 
  [ 
    "test7" >:: test9;
  ]

let () = 
  ignore (run_test_tt_main suite)
