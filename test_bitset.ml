open OUnit

let test1 test_ctxt = 
  let x = Bitset.empty ~size:8 in
  let y = Bitset.to_bitfield x in
  assert_equal "\x00" (Bitfield.to_string y)

let test2 test_ctxt = 
  let x = Bitset.empty ~size:8 in
  Bitset.insert x 0;
  let y = Bitset.to_bitfield x in
  assert_equal "\x80" (Bitfield.to_string y)

let test3 test_ctxt = 
  let x = Bitset.empty ~size:8 in
  Bitset.insert x 7;
  let y = Bitset.to_bitfield x in
  assert_equal "\x01" (Bitfield.to_string y)

let test4 test_ctxt =
  let x = Bitset.empty ~size:42 in
  for i = 0 to (Bitset.size x) - 1 do 
    assert_equal (Bitset.belongs x i) false
  done

let test5 test_ctxt =
  let x = Bitset.empty ~size:42 in
  Bitset.insert x 10;
  assert_equal (Bitset.belongs x 10) true

let test6 test_ctxt = 
  let x = Bitset.empty ~size:9 in
  Bitset.insert x 7;
  let y = Bitset.to_bitfield x in
  assert_equal "\x01\x00" (Bitfield.to_string y)

let test7 test_ctxt = 
  let x = Bitset.empty ~size:9 in
  Bitset.insert x 8;
  Bitset.insert x 7;
  let y = Bitset.to_bitfield x in
  assert_equal "\x01\x80" (Bitfield.to_string y)

let test8 test_ctxt = 
  let x = Bitset.empty ~size:9 in
  Bitset.insert x 8;
  Bitset.insert x 7;
  let s = Bitfield.of_string "\x01\x80" in
  let z = Bitset.empty ~size:9 in  
  Bitset.insert_from_bitfield z s;
  assert_equal x z

let suite =
  "suite">:::
   ["test1">:: test1;
    "test2">:: test2;
    "test3">:: test3;
    "test4">:: test4;
    "test5">:: test5;
    "test6">:: test6;
    "test7">:: test7;
    "test8">:: test7;
   ]

let () = 
  ignore (run_test_tt_main suite)
