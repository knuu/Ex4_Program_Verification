(**
Exercise 7.2 Ϳ����줿���Ȥλؤ���������� 1 ���䤹�ؿ� incr ���������.
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
