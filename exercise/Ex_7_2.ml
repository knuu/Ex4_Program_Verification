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
(* �񤯤��� *)

(*
let x = ref 3 in
incr x;
!x = 4;;
 *)

(* ñ�ʤ�ư���ǧ *)
