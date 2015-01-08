(**
Exercise 7.2 与えられた参照の指す先の整数を 1 増やす関数 incr を定義せよ.
    # let x = ref 3;;
    val x : int ref = {contents = 3}
# incr x;;
- : unit = ()
# !x;;
- : int = 4
 *)

let incr x =
  x := !x + 1
;;
(* 書くだけ *)

(*
let x = ref 3 in
incr x;
!x = 4;;
 *)

(* 単なる動作確認 *)
