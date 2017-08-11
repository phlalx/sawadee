open OUnit

let test1 test_ctxt = 
  let x = Bitfield.empty 64 in
  for i = 0 to 63 do 
    assert_equal false (Bitfield.get x i);
    Bitfield.set x i true;
    assert_equal true (Bitfield.get x i)
  done 

let test2 test_ctxt = 
  let x = Bitfield.empty 64 in
  for i = 0 to 7 do 
    Bitfield.set x i true
  done;
  let s = Bitfield.to_string x in
  assert_equal  s.[0] '\255'

let test3 test_ctxt = 
  let x = Bitfield.empty 64 in
  for i = 56 to 63 do 
    Bitfield.set x i true 
  done;
  let s = Bitfield.to_string x in
  assert_equal  s.[7] '\255'

let test4 test_ctxt = 
  let s = "\129\001" in
  let x = Bitfield.of_string s in
  let l = Bitfield.to_list x 16 in
  assert_equal l [0; 7; 15]

let test5 test_ctxt = 
  let s = "\129\000" in
  let x = Bitfield.of_string s in
  let l = Bitfield.to_list x 7 in
  assert_equal l [0]

let test6 test_ctxt = 
  let s = "\129\000" in
  let x = Bitfield.of_string s in
  let c = Bitfield.card x in
  Core.Printf.printf "%d" c;
  assert_equal c 2 

let test7 test_ctxt = 
  let s = "\129\001" in
  let x = Bitfield.of_string s in
  let c = Bitfield.card x in
  assert_equal c 3 

let test8 test_ctxt = 
  let s = "\128\000" in
  let x = Bitfield.of_string s in
  Bitfield.set x 7 true; 
  Bitfield.set x 15 true; 
  Bitfield.set x 14 true; 
  let y = Bitfield.to_string x in
  assert_equal y "\129\003"; 
  let c = Bitfield.card x in
  assert_equal c 4

let test9 test_ctxt = 
  let s = "\129\003" in
  let x = Bitfield.of_string s in
  Bitfield.set x 7 false; 
  Bitfield.set x 15 false; 
  Bitfield.set x 14 false; 
  let y = Bitfield.to_string x in
  assert_equal y "\128\000" 

let test10 test_ctxt = 
  let s1 = "\205\198" in
  let s2 = "\255\255" in
  let b1 = Bitfield.of_string s1 in
  let b2 = Bitfield.of_string s2 in
  assert_equal true (Bitfield.is_subset 2 b1 b2)

let test11 test_ctxt = 
  let s1 = "\205\198" in
  let s2 = "\255\255" in
  let b1 = Bitfield.of_string s1 in
  let b2 = Bitfield.of_string s2 in
  assert_equal true (Bitfield.is_subset 1 b1 b2)

let test12 test_ctxt = 
  let s1 = "\205\198" in
  let s2 = "\255\255" in
  let b1 = Bitfield.of_string s1 in
  let b2 = Bitfield.of_string s2 in
  assert_equal false (Bitfield.is_subset 2 b2 b1)

let test13 test_ctxt = 
  let s1 = "\205\198" in
  let s2 = "\255\255" in
  let b1 = Bitfield.of_string s1 in
  let b2 = Bitfield.of_string s2 in
  assert_equal false (Bitfield.is_subset 1 b2 b1)

let suite =
  "suite">:::
  [
    "test1">:: test1;
    "test2">:: test2;
    "test3">:: test3;
    "test4">:: test4;
    "test5">:: test5;
    "test6">:: test6;
    "test7">:: test7;
    "test8">:: test8;
    "test9">:: test9;
    "test10">:: test10;
    "test11">:: test11;
    "test12">:: test12;
    "test13">:: test13;
  ]

let () = 
  ignore (run_test_tt_main suite)
