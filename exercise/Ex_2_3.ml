(** Ex 2.3 *)

(* (1) 演算子の優先順位の問題なので、括弧をつける *)
not (true && false);;

(* (2) 関数の引数がおかしいので括弧をつける *)
float_of_int (int_of_float 5.0);;

(* (3) 関数の引数がおかしいので括弧をつける *)
(sin (3.14 /. 2.0)) ** 2.0 +. (cos (3.14 /. 2.0)) ** 2.0;;

(* (4) sqrtは float -> float なので、引数をfloat型になおし、返り値をint型に返す*)
int_of_float (sqrt (float_of_int (3 * 3 + 4 * 4)));;
