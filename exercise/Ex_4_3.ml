(** Ex 4.3 *)
(*
int->int->int->int
����: int��3��
�֤���: int��
 *)
let plus_3 a b c = (* 3��­�� *)
  a + b + c
;;

(*
(int->int)->int->int
����: �ؿ�(����:int��1�� �֤���:int��)1�ġ�int��1��
�֤���: int��
 *)
let foo f a =
  f 1 + a + 1
;;

(*
(int->int->int)->int
����: �ؿ�(����:int��2�� �֤���:int��)1��
�֤���: int��
 *)
let bar f =
  f 1 2 + 3
;;
