open OUnit

let test1 test_ctxt = 
  let x = Bitset.create 8 in
  let y = Bitset.to_string x in
  assert_equal "\x00" y

let test2 test_ctxt = 
  let x = Bitset.create 8 in
  Bitset.set x 0 true;
  let y = Bitset.to_string x in
  assert_equal "\x80" y

let test3 test_ctxt = 
  let x = Bitset.create 8 in
  Bitset.set x 7 true;
  let y = Bitset.to_string x in
  assert_equal "\x01" y

let test4 test_ctxt =
  let x = Bitset.create 42 in
  for i = 0 to (Bitset.length x) - 1 do 
    assert_equal (Bitset.get x i) false
  done

let test5 test_ctxt =
  let x = Bitset.create 42 in
  Bitset.set x 10 true;
  assert_equal (Bitset.get x 10) true

let test6 test_ctxt = 
  let x = Bitset.create 9 in
  Bitset.set x 7 true;
  let y = Bitset.to_string x in
  assert_equal "\x01\x00" y

let test7 test_ctxt = 
  let x = Bitset.create 9 in
  Bitset.set x 8 true;
  Bitset.set x 7 true;
  let y = Bitset.to_string x in
  assert_equal "\x01\x80" y

let test8 test_ctxt = 
  let x = Bitset.create 9 in
  Bitset.set x 8 true;
  Bitset.set x 7 true;
  let s = "\x01\x80" in
  let z = Bitset.create 9 in  
  Bitset.fill_from_string z s;
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
