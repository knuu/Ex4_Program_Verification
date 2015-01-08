(** 
Exercise 6.2 nat �����ͤ򤽤줬ɽ������ int ���Ѵ�����ؿ� int_of_nat, nat ��γݤ�����Ԥ��ؿ� mul,nat ��ΰ�������Ԥ��ؿ� (������ 0 - n = 0) monus (�⡼�ʥ�) ���������.(mul, monus �� *, - �ʤɤν�����ڤꤺ,nat �����ͤ����ľ�ܡ׷׻�����褦�ˤ���.)
 *)
type nat = Zero | OneMoreThan of nat;;

let rec add m n =
  match m with Zero -> n | OneMoreThan m' -> OneMoreThan (add m' n)
;;

let rec int_of_nat n = 
  match n with
  | Zero -> 0
  | OneMoreThan n' -> succ (int_of_nat n')
;;

let rec mul m n = 
  match m with
  | Zero -> Zero
  | OneMoreThan m' -> add n (mul m' n)
;;

let rec monus m n =
  match m with
  | Zero -> Zero
  | OneMoreThan m' -> match n with
		      | Zero -> m
		      | OneMoreThan n' -> monus m' n'
;;

(* Zero�������Ǥʤ����Ǿ��ʬ�����ƺƵ�Ū�˷׻����� *)

let zero = Zero
let one = OneMoreThan zero
let two = OneMoreThan one
let three = OneMoreThan two;;
(*
int_of_nat(mul two three) = 6;;
int_of_nat(monus three one) = 2;;
 *)

(* ñ�ʤ�ư���ǧ *)

