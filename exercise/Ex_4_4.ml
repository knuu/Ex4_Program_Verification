(** Ex 4.4 *)
let curry f x y = f (x, y);;

let uncurry f (x, y) = f x y
;;

(* �����Ȥ���(Coq�Ǥ��ä��̤�)�ʤΤ�������ά *)
(*
let average (x, y) = (x +. y) /. 2. in
(uncurry (curry average)) (4.0, 5.3) = average(4.0, 5.3);;
 *)
(* Ŭ���ʴؿ���������ơ����꡼�������󥫥꡼������Ф�Ȥ���뤳�Ȥγ�ǧ *)
