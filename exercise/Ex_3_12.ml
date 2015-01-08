(** 
Exercise 3.12 neg ��ñ�Ȥ��Ѥ���ɬ�פ��ʤ����,pos �� neg �ϰ�Ĥδؿ��ˤޤȤ�뤳�Ȥ��Ǥ���.��ĤˤޤȤ���������.
 *)
let rec leibniz_f n = (* Leibniz�θ��� *)
  if n < 0 then 0.0
  else if n = 0 then 1.0
  else 
    leibniz_f(n - 1) -. 1.0 /. float_of_int(4 * n - 1) +. 1.0 /. float_of_int(4 * n + 1)
;;

(* �פϿ����򥷥���1�Ĥǽ񤤤��Ȥ���Ʊ���ʤΤǤ��η׻��򤷤����� *)

(*
let rec pos n =
  neg (n-1) +. 1.0 /. (float_of_int (4 * n + 1))
and neg n =
  if n < 0 then 0.0
  else pos n -. 1.0 /. (float_of_int (4 * n + 3)) in
leibniz_f 800 = pos 800
 *)

(* ư���ǧ�ȤȤ�ˡ�pos��Ʊ�ͤν��Ϥ�Ԥ����γ�ǧ *)

