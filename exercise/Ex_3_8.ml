(**
Exercise 3.8 前問の pow の最初の定義を反復的にした powi を定義せよ.(もちろん引数の数は一つ増える.呼出し方も説明せよ.)
 *)
let rec powi(x, n, res) = (* 反復で累乗 *)
  if n = 0 then res
  else powi(x, n - 1, x *. res)
;;

(* 呼び出し方: 以下のようにpowを定義し、3.7と同じように引数を2つ渡す *)
let pow(x, n) = powi(x, n, 1.0);;

(* 末尾再帰に書きなおしただけ *)

(*
Ex_3_8.pow(3.14, 6) = Ex_3_7_2.pow(3.14, 6)
 *)

(* 動作確認とともに、3.7(2)と同等の結果が出るかの確認 *)
 
