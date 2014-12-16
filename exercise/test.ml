open OUnit2;;

let ex_2_1 =
  let test1 _ = assert_equal (float_of_int 3 +. 2.5) 5.5 in 
  let test2 _ = assert_equal (if "11" > "100" then "foo" else "bar") "foo" in
  let test3 _ = assert_equal (char_of_int ((int_of_char 'A') + 20)) 'U' in
  let test4 _ = assert_equal (int_of_string "0xff") 255 in
  let test5 _ = assert_equal (5.0 ** 2.0) 25. in
  "ex_2_1">:::
    ["ex_2_1_1">:: test1;
     "ex_2_1_2">:: test2;
     "ex_2_1_3">:: test3;
     "ex_2_1_4">:: test4;
     "ex_2_1_5">:: test5]
;;

let ex_2_3 =
  let test1 _ = assert_equal (not (true && false)) true in
  let test2 _ = assert_equal (float_of_int (int_of_float 5.0)) 5.0 in
  let test3 _ = assert_equal ((sin (3.14 /. 2.0)) ** 2.0 +. (cos (3.14 /. 2.0)) ** 2.0) 1.0 in
  let test4 _ = assert_equal (int_of_float (sqrt (float_of_int (3 * 3 + 4 * 4)))) 5 in
  "ex_2_3">:::
    ["ex_2_3_1">:: test1;
     "ex_2_3_2">:: test2;
     "ex_2_3_3">:: test3;
     "ex_2_3_4">:: test4]
;;
		     
let ex_2_4 =
  let test1 _ = 
    let b1 = true and b2 = true in
    assert_equal (b1 && b2) (if b1 then b2 else false) in
  let test2 _ = 
    let b1 = true and b2 = false
    in assert_equal (b1 && b2) (if b1 then b2 else false) in
  let test3 _ = 
    let b1 = false and b2 = true
    in assert_equal (b1 && b2) (if b1 then b2 else false) in
  let test4 _ = 
    let b1 = false and b2 = false
    in assert_equal (b1 && b2) (if b1 then b2 else false) in
  let test5 _ = 
    let b1 = true and b2 = true
    in assert_equal (b1 || b2) (if b1 then true else b2) in
  let test6 _ = 
    let b1 = true and b2 = false
    in assert_equal (b1 || b2) (if b1 then true else b2) in
  let test7 _ = 
    let b1 = false and b2 = true
    in assert_equal (b1 || b2) (if b1 then true else b2) in
  let test8 _ = 
    let b1 = false and b2 = false
    in assert_equal (b1 || b2) (if b1 then true else b2) in
  "ex_2_4">:::
    ["and_test1">:: test1;
     "and_test2">:: test2;
     "and_test3">:: test3;
     "and_test4">:: test4;
     "or_test1">:: test5;
     "or_test2">:: test6;
     "or_test3">:: test7;
     "or_test4">:: test8]
;;

let ex_2_6 =
  let test1_1 _ = assert_equal (Ex_2_6.dollar_to_yen 112.0) 12445 in
  let test1_2 _ = assert_equal (Ex_2_6.dollar_to_yen 113.0) 12557 in
  let test2_1 _ = assert_equal (Ex_2_6.yen_to_dollar 49390) 444.47 in
  let test2_2 _ = assert_equal (Ex_2_6.yen_to_dollar 12346) 111.11 in
  let test3 _ = assert_equal (Ex_2_6.exchange_d_to_y 112.0) "112. dollars are 12445 yen." in
  let test4_1 _ = assert_equal (Ex_2_6.capitalize 'c') 'C' in
  let test4_2 _ = assert_equal (Ex_2_6.capitalize 'C') 'C' in
  "ex_2_6">:::
    ["ex_2_6_1(foo.4)">:: test1_1;
     "ex_2_6_1(foo.5)">:: test1_2;
     "ex_2_6_2(foo.ab4)">:: test2_1;
     "ex_2_6_2(foo.ab5)">:: test2_2;
     "ex_2_6_3">:: test3;
     "ex_2_6_4(lower)">:: test4_1;
     "ex_2_6_4(not lower)">:: test4_2]
;;

let ex_3_1 =
  let test1 _ = assert_equal (Ex_3_3.geo_mean (100., 10000.)) 1000. in
  let test2 _ = assert_equal (Ex_3_3.geo_mean (2., 2.)) ((2. +. 2.) /. 2.) in
  "ex_3_1">:::
    ["geo_mean 10^2 10^4 = 10^3">:: test1;
     "geo_mean a a = arith_mean a a">:: test2]
;;

let ex_3_2 =
  let test _ = assert_equal (Ex_3_4.prodMatVec (((1., 2.), (3., 4.)), (5., 6.))) (17., 39.) in
  "ex_3_2">:::
    ["prodMatVec">:: test]
;;

let ex_3_7 =
  let pow1 _ = assert_equal (Ex_3_7_1.pow(3., 4)) 81. in
  let pow2 _ = assert_equal (Ex_3_7_2.pow(3., 4)) 81. in
  "ex_3_7">:::
    ["pow_rec_n">:: pow1;
     "pow_rec_log">:: pow2]
;;

let ex_3_8 =
  let pow _ = assert_equal (Ex_3_8.pow(3.14, 6)) (Ex_3_7_2.pow(3.14, 6)) in
  "ex_3_8">:::
    ["pow_log = pow_iter">:: pow]
;;
			    
let ex_3_11 =
  let rec fib n =
    if n = 1 || n = 2 then 1 else fib (n - 1) + fib (n - 2) in
  let gcd _ = assert_equal (Ex_3_11.gcd(4885, 6839)) 977 in
  let comb1 _ = assert_equal (Ex_3_11.comb(6, 3)) 20 in
  let comb2 _ = assert_equal (Ex_3_11.comb(25, 12)) (Ex_3_11.comb(25, 13)) in
  let fib_iter1 _ = assert_equal (Ex_3_11.fib_iter 10) 55 in
  let fib_iter2 _ = assert_equal (Ex_3_11.fib_iter 10) (fib 10) in
  let max_ascii _ = assert_equal (Ex_3_11.max_ascii "String") 't' in
  "ex_3_11">:::
    ["gcd">:: gcd;
     "comb 6C3=20">:: comb1;
     "comb 25C12=25C13">:: comb2;
     "fib_iter10=55">:: fib_iter1;
     "fib_iter10=fib10">:: fib_iter2;
     "max_ascii'String'='t'">:: max_ascii]
;;
	
let () =
  run_test_tt_main ex_2_1;
  run_test_tt_main ex_2_3;
  run_test_tt_main ex_2_4;
  run_test_tt_main ex_2_6;
  run_test_tt_main ex_3_1;
  run_test_tt_main ex_3_2;
  run_test_tt_main ex_3_7;
  run_test_tt_main ex_3_8;
  run_test_tt_main ex_3_11;
;;