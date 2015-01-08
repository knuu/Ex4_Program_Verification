(**
Exercise 4.5 以下の関数 repeat は double, fourtimes などを一般化したもので,f を n 回,x に適用する関数である.
    # let rec repeat f n x =
        if n > 0 then repeat f (n - 1) (f x) else x;;
    val repeat : ('a -> 'a) -> int -> 'a -> 'a = <fun>
これを使って,フィボナッチ数を計算する関数 fib を定義する.以下の ... の部分を埋めよ.
    let fib n =
      let (fibn, _) = ...
      in fibn;;
 *)

let rec repeat f n x = (* f を n 回 x に適用する *)
  if n > 0 then repeat f (n - 1) (f x) else x;;

let fib n = 
  let (fibn, _) = 
    repeat (fun (x, y) -> (x + y, x)) n (0, 1)
  in fibn;;

(* 32ページのfib_pairのアイデアを元に実装 *)
(*
fib 1000 = Ex_3_11.fib_iter 1000;;
 *)
(* 動作確認 & 3.11のfib_iterと同等の出力か確認 *)

