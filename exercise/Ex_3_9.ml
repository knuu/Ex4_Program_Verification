(** 
Exercise 3.9 if ���� Objective Caml �δؿ���ɽ�����뤳�ȤϤǤ��ʤ�.�ʲ��δؿ��Ϥ�����ߤ���ΤǤ���.fact 4 �η׻���ɾ�����ƥåפ�ͤ�,�ʤ����ޤ��׻��Ǥ��ʤ��Τ���������.
# let cond (b, e1, e2) : int = if b then e1 else e2;;
  val cond : bool * int * int -> int = <fun>
# let rec fact n = cond ((n = 1), 1, n * fact (n-1));;
  val fact : int -> int = <fun>
# fact 4;;
  ???? 
 *)
(*
let cond (b, e1, e2) : int  = if b then e1 else e2;;
let rec fact n = cond ((n = 1), 1, n * fact (n - 1));;
 *)
(* ������fact 4��׻�����ȡ�̵�¥롼�פ򵯤����� �ʤ��ʤ�OCaml�Ǥ�ɾ����ά�Ȥ��ơ�����p.29���͸ƽФ�����ά��ȤäƤ��뤫��Ǥ��롣����ϡ��ؿ���Ŭ�Ѥ���Ȥ��ˤϤޤ����������ɾ������Ȥ�����ΤǤ��ꡢfact 4�ǤߤƤߤ�ȡ�
fact 4 (* �����ϴ���ɾ������Ƥ���Τ�fact��Ÿ�� *)
-> cond ((4 = 1), 1, 4 * fact 3) (* ������ɾ�� *)
-> cond (true, 1, 4 * cond((3 = 1), 1, fact 2)) 
-> ��
�Ȥ����褦�ˡ�������fact�ΰ�����̵�¤˾������ʤäƤ��äƤ��ޤ���
��ä�̵�¥롼�פ򵯤��������ޤ��׻�����ʤ���
 *)
