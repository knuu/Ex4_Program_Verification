(**
Exercise 6.3 ��� monus �ؿ����ѹ�����,0-n (n > 0) �� None ���֤� nat -> nat -> nat option ���δؿ� minus ���������.
 *)

type 'a option = None | Some of 'a;;

let rec monus m n = 
  match m with
  | Ex_6_2.Zero -> if n = Ex_6_2.Zero then Some Ex_6_2.Zero else None
  | Ex_6_2.OneMoreThan m' -> match n with
		      | Ex_6_2.Zero -> Some m
		      | Ex_6_2.OneMoreThan n' -> monus m' n'
;;

(* �֤��ͤ�option���˽񤭴��������� *)

let rec get_value op default =
  match op with
  | None -> default
  | Some n -> n
;;
(*
monus Ex_6_2.two Ex_6_2.three = None;;
get_value (monus Ex_6_2.three Ex_6_2.two) Ex_6_2.Zero = Ex_6_2.monus' Ex_6_2.three Ex_6_2.two;;
 *)

(* None�ˤʤ���Υƥ��Ȥ� �����Ǥʤ����Υƥ��� *)
