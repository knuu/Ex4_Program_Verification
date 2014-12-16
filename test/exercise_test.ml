open OUnit2;;

let ex_2_1 =
  let test1 _ = assert_equal (float_of_int 3 +. 2.5) 5.5 in 
  let test2 _ = assert_equal (if "11" > "100" then "foo" else "bar") "foo" in
  let test3 _ = assert_equal ((int_of_char 'A') + 20) 'U' in
  let test4 _ = assert_equal (int_of_string "0xff") 225 in
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
    let b1 = true and b2 = true
    in assert_equal (b1 && b2) (if b1 then b2 else false) in
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
    ["and_test1"::> test1;
     "and_test2"::> test2;
     "and_test3"::> test3;
     "and_test4"::> test4;
     "or_test1"::> test5;
     "or_test2"::> test6;
     "or_test3"::> test7;
     "or_test4"::> test8]
;;

let ex_2_6 =
  let test1_1 _ = assert_equal (Ex_2_6.dollar_to_yen 112.) 12445 in
  let test1_2 _ = assert_equal (Ex_2_6.dollar_to_yen 123.) 12557 in
  let test2_1 _ = assert_equal (Ex_2_6.yen_to_dollar 12362) 111.2 in
  let test2_2 _ = assert_equal (Ex_2_6.yen_to_dollar 12363) 111.3 in
  let test3 _ = assert_equal (Ex_2_6.exchange_d_to_y 112.) "112. dollars are 12445 yen." in
  let test4_1 _ = assert_equal (Ex_2_6.capitalize 'c') 'C' in
  let test4_2 _ = assert_equal (Ex_2_6.capitalize 'C') 'C' in
  "ex_2_6">:::
    ["ex_2_6_1 四捨">:: test1_1;
     "ex_2_6_1 五入">:: test1_2;
     "ex_2_6_2 四捨">:: test2_1;
     "ex_2_6_2 五入">:: test2_2;
     "ex_2_6_3">:: test3;
     "ex_2_6_4 小文字">:: test4_1;
     "ex_2_6_4 小文字じゃない">:: test4_2]
;;

			     

let () =
  run_test_tt_main ex_2_1;
  run_test_tt_main ex_2_3;
  run_test_tt_main ex_2_4;
  run_test_tt_main ex_2_6;
;;
