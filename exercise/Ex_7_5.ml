(**
Exercise 7.5 階乗関数 fact を負の引数に対して Invalid_argument を発生させるように改良せよ.
 *)

exception Invalid_argument;;

let rec fact n =
  if n < 0 then raise Invalid_argument
  else if n = 0 then 1 else n * fact (n - 1)
;;

(* 場合分けで、負の数のときに例外を投げる *)
(*
(try fact 5 with Invalid_argument -> 0) = 120;;
(try fact (-10) with Invalid_argument -> 0) = 0;;
 *)
(* 例外を投げる場合と投げない場合でテスト *)
