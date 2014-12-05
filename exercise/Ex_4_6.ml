(** Ex 4.6 *)
(* 
funny は f を n 回 適用する関数である
これはlogのオーダーで累乗を呼び出す関数と同じような構造をしており、
実際同じように動作する。
動作テストを見れば、上で述べたことが正しいのは明らか。
 *)

let id x = x;;

let ($) f g x = f (g x);;

let rec funny f n =
  if n = 0 then id
  else if n mod 2 = 0 then funny (f $ f) (n / 2)
  else funny (f $ f) (n / 2) $ f
;;

(* テスト *)
(*
let rec funny2 f n =
  if n = 0 then id
  else f $ (funny2 f (n - 1))
;;

let succ n = n + 1;;

funny succ 10 5 = funny2 succ 10 5;;
 *)
