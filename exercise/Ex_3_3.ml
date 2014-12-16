(** 
Exercise 3.3 2 実数の相乗平均をとる関数 geo_mean を定義せよ.
 *)
let geo_mean pair = (* 相乗平均 *)
  let (x, y) = pair in sqrt(x *. y);;
