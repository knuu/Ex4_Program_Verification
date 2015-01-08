(** 
Exercise 3.3 2 実数の相乗平均をとる関数 geo_mean を定義せよ.
 *)
let geo_mean pair = (* 相乗平均 *)
  let (x, y) = pair in sqrt(x *. y);;

(* 定義通りに計算しただけ *)

(*
geo_mean (100., 10000.) = 1000.;;
geo_mean (2., 2.) = (2. +. 2.) /. 2.;;
 *)

(*
1つ目は動作確認
2つ目は相加平均と相乗平均が等しくなる値でテスト
 *)
