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

(* Test *)
let x = ref 3;;
for i = 1 to 10 do incr x done;;
!x = 13;;
