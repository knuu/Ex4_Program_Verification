(**
Exercise 4.6 次の関数 funny がどのような働きをするか説明せよ.
    # let rec funny f n =
        if n = 0 then id
        else if n mod 2 = 0 then funny (f $ f) (n / 2)
        else funny (f $ f) (n / 2) $ f;;
    val funny : ('a -> 'a) -> int -> 'a -> 'a = <fun>
 *)
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

let rec funny_rec f n = (* 線形オーダー版funny *)
  if n = 0 then id
  else f $ (funny_rec f (n - 1))
;;
