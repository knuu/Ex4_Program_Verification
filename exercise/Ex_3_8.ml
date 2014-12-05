(** Ex 3.8 *)
let rec powi(x, n, res) = (* 反復で累乗 *)
  if n = 0 then res
  else powi(x, n - 1, x *. res)
;;

(* 呼び出し方: 以下のようにpowを定義し、3.7と同じように引数を2つ渡す *)
let pow(x, n) = powi(x, n, 1.0);;
