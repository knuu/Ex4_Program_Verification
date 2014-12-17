(** 
Exercise 4.2 �������� 3.7 �� pow �ؿ��򥫥꡼�������������. �������������ؿ��ˤʤ�褦 (pow n x) �������,3 �褹��ؿ� cube ����ʬŬ�Ѥ��������.�ؿ�����������Ǥ���褦���������Ƥ����� (pow x n),cube �� pow �����������ˤϤɤ�����Ф褤��?
 *)
let rec pow1 x n = (* ���꡼�����줿�߾�1 *)
  if n = 0 then 1.0 else x *. pow1 x (n-1)
;;

let rec pow2 n x = (* ���꡼�����줿�߾�2 *)
  if n = 0 then 1.0 else x *. pow2 (n-1) x
;;
  
let cube1 = (* pow2����cube����� *)
  pow2 3;;

let cube2 = (* pow1����cube����� *)
  (fun x -> pow1 x 3)
;;

(* �ƥ��� *)
(*
cube1 3. = cube2 3.;;
 *)
