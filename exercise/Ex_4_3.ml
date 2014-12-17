(**
Exercise 4.3 以下の 3 つの型
int -> int -> int -> int
(int -> int) -> int -> int 
(int -> int -> int) -> int
の違いを説明せよ.また,各型に属する適当な関数を定義せよ.
 *)
(*
int->int->int->int
引数: int型3つ
返り値: int型
 *)
let plus_3 a b c = (* 3つ足す *)
  a + b + c
;;

(*
(int->int)->int->int
引数: 関数(引数:int型1つ 返り値:int型)1つ、int型1つ
返り値: int型
 *)
let foo f a =
  f 1 + a + 1
;;

(*
(int->int->int)->int
引数: 関数(引数:int型2つ 返り値:int型)1つ
返り値: int型
 *)
let bar f =
  f 1 2 + 3
;;
