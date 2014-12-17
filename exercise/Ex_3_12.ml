(** 
Exercise 3.12 neg を単独で用いる必要がなければ,pos と neg は一つの関数にまとめることができる.一つにまとめて定義せよ.
 *)
let rec leibniz_f n = (* Leibnizの公式 *)
  if n < 0 then 0.0
  else if n = 0 then 1.0
  else 
    leibniz_f(n - 1) -. 1.0 /. float_of_int(4 * n - 1) +. 1.0 /. float_of_int(4 * n + 1)
;;

(* テスト *)
(*
let rec pos n =
  neg (n-1) +. 1.0 /. (float_of_int (4 * n + 1))
and neg n =
  if n < 0 then 0.0
  else pos n -. 1.0 /. (float_of_int (4 * n + 3));;

pos 200 = leibniz_f 200;;
pos 800 = leibniz_f 800;;
 *)
