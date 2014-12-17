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

let ex_3_12 =
  let rec pos n =
    neg (n-1) +. 1.0 /. (float_of_int (4 * n + 1))
  and neg n =
    if n < 0 then 0.0
    else pos n -. 1.0 /. (float_of_int (4 * n + 3)) in
  let leibniz_f _ = assert_equal (Ex_3_12.leibniz_f 800) (pos 800) in
  "ex_3_12">:::
    ["Leibniz formula">:: leibniz_f]
;;

let ex_4_1 =
  let integral _ = assert_equal (Ex_4_1.integral (fun x -> x) 0. 1.) 0.5 in
  "ex_4_1">:::
    ["integral[0,1] xdx = 0.5">:: integral]
;;

let ex_4_2 =
  let cube _ = assert_equal (Ex_4_2.cube1 8.) (Ex_4_2.cube2 8.) in
  "ex_4_2">:::
    ["cube">:: cube]
;;

let ex_4_3 =
  let ex_4_3_1 _ = assert_equal (Ex_4_3.plus_3 1 2 3) 6 in
  let ex_4_3_2 _ = assert_equal (Ex_4_3.foo (fun x -> x) 1) 3 in 
  let ex_4_3_3 _ = assert_equal (Ex_4_3.bar (fun x y -> x + y)) 6 in
  "ex_4_3">:::
    ["ex_4_3_1">:: ex_4_3_1;
     "ex_4_3_2">:: ex_4_3_2;
     "ex_4_3_3">:: ex_4_3_3]
;;

let ex_4_4 =
  let average (x, y) = (x +. y) /. 2. in
  let curry_uncurry _ = assert_equal ((Ex_4_4.uncurry (Ex_4_4.curry average)) (4.0, 5.3)) (average(4.0, 5.3)) in
  "ex_4_4">:::
    ["curry_uncurry">:: curry_uncurry]
;;

let ex_4_5 =
  let fib _ = assert_equal (Ex_4_5.fib 1000) (Ex_3_11.fib_iter 1000) in
  "ex_4_5">:::
    ["fib">:: fib]
;;

let ex_4_6 =
  let funny1 _ = assert_equal (Ex_4_6.funny succ 10 5) 15 in
  let funny2 _ = assert_equal (Ex_4_6.funny succ 10 5) (Ex_4_6.funny_rec succ 10 5) in
  "ex_4_6">:::
    ["funny1">:: funny1;
     "funny2">:: funny2]
;;

let ex_4_7 =
  let combinator _ = assert_equal ((Ex_4_7.k (Ex_4_7.s Ex_4_7.k Ex_4_7.k)) 1 2) 2 in
  "ex_4_7">:::
    ["combinator">:: combinator]
;;

let ex_5_2 =
  let sum_list _ = assert_equal (Ex_5_2.sum_list [1;2;3;4;5;6;7;8;9;10]) 55 in
  let max_list _ = assert_equal (Ex_5_2.max_list [1;5;7;2;1;5;7;8;4;2;5;7]) 8 in
  "ex_5_2">:::
    ["sum_list">:: sum_list;
     "max_list">:: max_list]
;;

let ex_5_3 =
  let rec length = function
    | [] -> 0 
    | _ :: t -> succ (length t) in
  let downto0 _ = assert_equal (Ex_5_3.downto0 10) ([10;9;8;7;6;5;4;3;2;1;0]) in
  let roman1 _ = assert_equal (Ex_5_3.roman [(1000, "M"); (500, "D"); (100, "C"); (50, "L"); (10, "X"); (5, "V"); (1, "I")] 1984) ("MDCCCCLXXXIIII") in
  let roman2 _ = assert_equal (Ex_5_3.roman [(1000, "M"); (900, "CM"); (500, "D"); (400, "CD"); (100, "C"); (90, "XC"); (50, "L"); (40, "XL"); (10, "X"); (9, "IX"); (5, "V"); (4, "IV"); (1, "I")] 1984) ("MCMLXXXIV") in
  let concat _ = assert_equal (Ex_5_3.concat [[0; 3; 4]; [2]; [5; 0]; []]) ([0; 3; 4; 2; 5; 0]) in
  let zip1 _ = assert_equal (Ex_5_3.zip [1;2;3] [2;3;4]) ([(1,2);(2,3);(3,4)]) in
  let zip2 _ = assert_equal (Ex_5_3.zip [1;1;1;1] [1;1]) (Ex_5_3.zip [1;1] [1;1;1;1]) in
  let zip3 _ = assert_equal (Ex_5_3.zip [] [1]) (Ex_5_3.zip [1] []) in
  let filter1 _ = assert_equal (Ex_5_3.filter (fun x -> x > 0) [-9; 0; 2; 5; -3]) ([2; 5]) in
  let filter2 _ = assert_equal (Ex_5_3.filter (fun l -> length l = 3) [[1; 2; 3]; [4; 5]; [6; 7; 8]; [9]]) ([[1; 2; 3]; [6; 7; 8]]) in
  let belong _ = assert_equal (Ex_5_3.belong [3;4] [[1];[2;3];[3;4];[3;4;5]]) (not (Ex_5_3.belong [] [[1];[2];[3]])) in
  let intersect _ = assert_equal (Ex_5_3.intersect [1;4;7;8;9;10] [4;7;1;0;3]) ([1;4;7]) in
  let union _ = assert_equal (Ex_5_3.union [1;2;3;4] [2;3;4;5]) ([1;2;3;4;5]) in
  let diff _ = assert_equal (Ex_5_3.diff [1;2;3;4] [2;4;6;7]) ([1;3]) in
  "ex_5_3">:::
    ["downto0">:: downto0;
     "roman1">:: roman1;
     "roman2">:: roman2;
     "concat">:: concat;
     "zip same_len">:: zip1;
     "zip different_len">:: zip2;
     "zip zero_len">:: zip3;
     "filter1">:: filter1;
     "filter2">:: filter2;
     "belong">:: belong;
     "intersect">:: intersect;
     "union">:: union;
     "diff">:: diff]
;;

let ex_5_4 = 
  let f x = x + 1 and g x = x * 2 in
  let map _ = assert_equal (Ex_5_4.map (fun x -> f (g x)) [0;1;2;3;4;5;6;7;8;9;10]) (Ex_5_4.map f (Ex_5_4.map g [0;1;2;3;4;5;6;7;8;9;10])) in
  "ex_5_4">:::
    ["map f g">:: map]
;;

let ex_5_5 =
  let forall _ = assert_equal (Ex_5_5.forall (fun c -> 'z' > c) ['A'; ' '; '+']) (true) in
  let exists _ = assert_equal (Ex_5_5.exists (fun x -> (x mod 7) = 0) [23; -98; 19; 53]) (true) in
  "ex_5_5">:::
    ["forall">:: forall;
     "exists">:: exists]
;;

let ex_5_6 =
  let l1 = snd(Ex_5_6.randlist 50 1.0 []) in
  let l2 = snd(Ex_5_6.randlist 10000 1.0 []) in
  let quick1 _ = assert_equal (Ex_5_6.quicker l1 []) (Ex_5_6.quick l1) in
  let quick2 _ = assert_equal (Ex_5_6.quicker l2 []) (Ex_5_6.insertion_sort l2) in
  "ex_5_6">:::
    ["quicker and quick">:: quick1;
     "quicker and insert">:: quick2]
;;

let ex_5_7 =
  let square _ = assert_equal (Ex_5_7.length (Ex_5_7.squares 48612265)) 32 in
  "ex_5_7">:::
    ["square">:: square]
;;

let ex_5_8 =
  let map2 _ = assert_equal (Ex_5_8.map (fun x -> x * 2) [4; 91; 0; -34]) (Ex_5_8.map2 (fun x -> x * 2) [4; 91; 0; -34]) in
  "ex_5_8">:::
    ["map2">:: map2]
;;

let ex_6_2 =
  let mul _ = assert_equal (Ex_6_2.int_of_nat(Ex_6_2.mul Ex_6_2.two Ex_6_2.three)) 6 in
  let monus _ = assert_equal (Ex_6_2.int_of_nat(Ex_6_2.monus Ex_6_2.three Ex_6_2.one)) 2 in
  "ex_6_2">:::
    ["mul two three">:: mul;
     "monus three one">:: monus]
;;
			  
let ex_6_3 =
  let monus1 _ = assert_equal (Ex_6_3.monus Ex_6_3.two Ex_6_3.three) (Ex_6_3.None) in
  let monus2 _ = assert_equal (Ex_6_3.get_value (Ex_6_3.monus Ex_6_3.three Ex_6_3.two) Ex_6_3.Zero) (Ex_6_3.monus' Ex_6_3.three Ex_6_3.two) in
  "ex_6_3">:::
    ["monus two three = None">:: monus1;
     "monus equal">:: monus2]
;;

let ex_6_4 =
  let comptree _ = assert_equal (Ex_6_4.comptree 1 3) (Ex_6_4.comptree3) in
  let depth _ = assert_equal (Ex_6_4.depth (Ex_6_4.comptree 1 3)) 3 in
  "ex_6_4">:::
    ["comptree">:: comptree;
     "depth comptree">:: depth]
;;

let ex_6_5 =
  let inord _ = assert_equal (Ex_6_5.inord Ex_6_5.comptree3 []) ([4; 2; 5; 1; 6; 3; 7]) in
  let postord _ = assert_equal (Ex_6_5.postord Ex_6_5.comptree3 []) ([4; 5; 2; 6; 7; 3; 1]) in
  "ex_6_5">:::
    ["inord">:: inord;
     "postord">:: postord]
;;

let ex_6_6 =
  let reflect _ = assert_equal (Ex_6_6.reflect Ex_6_6.comptree3) (Ex_6_6.rev_comptree3) in
  let order1 _ = assert_equal (Ex_6_6.preorder(Ex_6_6.reflect(Ex_6_6.comptree3))) (Ex_6_6.rev(Ex_6_6.postorder(Ex_6_6.comptree3))) in
  let order2 _ = assert_equal (Ex_6_6.inorder(Ex_6_6.reflect(Ex_6_6.comptree3))) (Ex_6_6.rev(Ex_6_6.inorder(Ex_6_6.comptree3))) in
  let order3 _ = assert_equal (Ex_6_6.postorder(Ex_6_6.reflect(Ex_6_6.comptree3))) (Ex_6_6.rev(Ex_6_6.preorder(Ex_6_6.comptree3))) in
  "ex_6_6">:::
    ["reflect">:: reflect;
     "preorder(reflect) = rev(postorder)">:: order1;
     "inorder(reflect) = rev(inorder)">:: order2;
     "postorder(reflect) = rev(preorder)">:: order3]
;;

let ex_6_8 =
  let bst1' _ = assert_equal (Ex_6_8.bst1') (Ex_6_8.bst1) in
  let bst2' _ = assert_equal (Ex_6_8.bst2') (Ex_6_8.bst2) in
  let bst3' _ = assert_equal (Ex_6_8.bst3') (Ex_6_8.bst3) in
  let bst4' _ = assert_equal (Ex_6_8.bst4') (Ex_6_8.bst3) in
  let bst5' _ = assert_equal (Ex_6_8.bst5') (Ex_6_8.bst4) in
  let bst6' _ = assert_equal (Ex_6_8.bst6') (Ex_6_8.bst5) in
  let bst7' _ = assert_equal (Ex_6_8.bst7') (Ex_6_8.bst6) in
  let bst8' _ = assert_equal (Ex_6_8.bst8') (Ex_6_8.bst7) in
  let bst9' _ = assert_equal (Ex_6_8.bst9') (Ex_6_8.bst6) in
  let bst10' _ = assert_equal (Ex_6_8.bst10') (Ex_6_8.bst6) in
  let bst11' _ = assert_equal (Ex_6_8.bst11') (Ex_6_8.bst7) in
  let bst12' _ = assert_equal (Ex_6_8.bst12') (Ex_6_8.bst7) in
  let bst13' _ = assert_equal (Ex_6_8.bst13') (Ex_6_8.bst8) in
  let bst14' _ = assert_equal (Ex_6_8.bst14') (Ex_6_8.bst8) in
  let bst15' _ = assert_equal (Ex_6_8.bst15') (Ex_6_8.bst9) in
  let bst16' _ = assert_equal (Ex_6_8.bst16') (Ex_6_8.bst9) in
  let bst17' _ = assert_equal (Ex_6_8.bst17') (Ex_6_8.bst8) in
  let bst18' _ = assert_equal (Ex_6_8.bst18') (Ex_6_8.bst9) in
  let bst19' _ = assert_equal (Ex_6_8.bst19') (Ex_6_8.bst10) in
  let bst20' _ = assert_equal (Ex_6_8.bst20') (Ex_6_8.bst11) in
  let bst21' _ = assert_equal (Ex_6_8.bst21') (Ex_6_8.bst12) in
  let bst22' _ = assert_equal (Ex_6_8.bst22') (Ex_6_8.bst12) in
  let bst23' _ = assert_equal (Ex_6_8.bst23') (Ex_6_8.bst13) in
  let bst24' _ = assert_equal (Ex_6_8.bst24') (Ex_6_8.bst14) in
  "ex_6_8">:::
    ["bst1'">:: bst1'; "bst2'">:: bst2'; "bst3'">:: bst3';
     "bst4'">:: bst4'; "bst5'">:: bst5'; "bst6'">:: bst6';
     "bst7'">:: bst7'; "bst8'">:: bst8'; "bst9'">:: bst9';
     "bst10'">:: bst10'; "bst11'">:: bst11'; "bst12'">:: bst12';
     "bst13'">:: bst13'; "bst14'">:: bst14'; "bst15'">:: bst15';
     "bst16'">:: bst16'; "bst17'">:: bst17'; "bst18'">:: bst18';
     "bst19'">:: bst19'; "bst20'">:: bst20'; "bst21'">:: bst21';
     "bst22'">:: bst22'; "bst23'">:: bst23'; "bst24'">:: bst24';]
;;  

let ex_6_9 =
  let seq _ = assert_equal (Ex_6_9.nthseq 10000 Ex_6_9.primes) 104729 in
  "ex_6_9">:::
    ["1000th prime">:: seq]
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
  run_test_tt_main ex_3_12;

  run_test_tt_main ex_4_1;
  run_test_tt_main ex_4_2;
  run_test_tt_main ex_4_3;
  run_test_tt_main ex_4_4;
  run_test_tt_main ex_4_5;
  run_test_tt_main ex_4_6;
  run_test_tt_main ex_4_7;

  run_test_tt_main ex_5_2;
  run_test_tt_main ex_5_3;
  run_test_tt_main ex_5_4;
  run_test_tt_main ex_5_5;
  run_test_tt_main ex_5_6;
  run_test_tt_main ex_5_7;
  run_test_tt_main ex_5_8;

  run_test_tt_main ex_6_2;
  run_test_tt_main ex_6_3;
  run_test_tt_main ex_6_4;
  run_test_tt_main ex_6_5;
  run_test_tt_main ex_6_6;
  run_test_tt_main ex_6_8;
  run_test_tt_main ex_6_9;
;;
