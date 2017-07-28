open OUnit
open Krpc_packet
open Bin_prot

let ping = {
  transaction_id = "abc";
  content = Query (Ping (Node_id.of_string "abcdefghijklmonopqrs"))
} 

let test1 test_ctxt = 
  assert_equal (t_of_bencode (bencode_of_t ping)) ping 

let test2 test_ctxt = 
  let buf = Common.create_buf buffer_size in 
  let len = bin_write_t buf ~pos:0 ping in
  let pos_ref = ref 0 in
  let _ = bin_read_t buf ~pos_ref len in 
  let ping' = ping in
  assert_equal ping ping'

let suite =
  "suite" >::: 
  [ "test1" >:: test1;
    "test2" >:: test2]

let () = 
  ignore (run_test_tt_main suite)
