(**
Exercise 4.6 ���δؿ� funny ���ɤΤ褦��Ư���򤹤뤫��������.
    # let rec funny f n =
        if n = 0 then id
        else if n mod 2 = 0 then funny (f $ f) (n / 2)
        else funny (f $ f) (n / 2) $ f;;
    val funny : ('a -> 'a) -> int -> 'a -> 'a = <fun>
 *)
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

let rec funny_rec f n = (* ��������������funny *)
  if n = 0 then id
  else f $ (funny_rec f (n - 1))
;;
