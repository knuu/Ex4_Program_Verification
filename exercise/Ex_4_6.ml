(** Ex 4.6 *)
(* 
funny �� f �� n �� Ŭ�Ѥ���ؿ��Ǥ���
�����log�Υ����������߾��ƤӽФ��ؿ���Ʊ���褦�ʹ�¤�򤷤Ƥ��ꡢ
�º�Ʊ���褦��ư��롣
ư��ƥ��Ȥ򸫤�С���ǽҤ٤����Ȥ��������Τ����餫��
 *)

let id x = x;;

let ($) f g x = f (g x);;

let rec funny f n =
  if n = 0 then id
  else if n mod 2 = 0 then funny (f $ f) (n / 2)
  else funny (f $ f) (n / 2) $ f
;;

(* �ƥ��� *)
(*
let rec funny2 f n =
  if n = 0 then id
  else f $ (funny2 f (n - 1))
;;

let succ n = n + 1;;

funny succ 10 5 = funny2 succ 10 5;;
 *)
