(** 
Exercise 7.3 �ʲ���������� funny_fact �ϺƵ�Ū��� (rec) ��Ȥ鷺�˳����׻����Ƥ���.�ɤΤ褦�ʻ��ȤߤǼ¸�����Ƥ��뤫��������.
# let f = ref (fun y -> y + 1)
  let funny_fact x = if x = 1 then 1 else x * (!f (x - 1));;
val f : (int -> int) ref = {contents = <fun>} val funny_fact : int -> int = <fun>
# f := funny_fact;;
- : unit = ()
# funny_fact 5;;
- : int = 120
 *)

(*
funny_fact�ؿ��ǡ������Ƶ�Ū�˷׻��򤹤���ʬ�ǸƤӽФ��ؿ�f�������Ѥ������ꤦ���ѿ��Ȥ���������Ƥ����ơ����θ塢f��funny_fact���������Ƥ��롣
�Ĥޤꡢñ��rec��Ȥ鷺��������Ƥ���Ȥ��������ǡ�funny_fact���ƤӽФ�������ʤɤ�(�����������Ǥʤ�)�Ƶ�Ū�����fact���Ѥ��ʤ���
 *)
