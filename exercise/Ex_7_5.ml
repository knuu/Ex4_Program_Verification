(**
Exercise 7.5 階乗関数 fact を負の引数に対して Invalid_argument を発生させるように改良せよ.
 *)

exception Invalid_argument;;

let rec fact n =
  if n < 0 then raise Invalid_argument
  else if n = 0 then 1 else n * fact (n - 1)
;;

