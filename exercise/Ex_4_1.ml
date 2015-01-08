(**
Exercise 4.1 �¿���δؿ� f ���Ф��� integral [a,b] f(x)dx ��׻�����ؿ� integral f a b ���������.
�ޤ������Ȥä�,integral [0,��] sin x dx ��׻�����
���Ū�ʷ׻���ˡ�Ȥ���,�����Ǥ����������������뤬¾����ˡ�Ǥ��ɤ�.��������Ǥ� b - a �� n ʬ�䤷����֤�Ĺ���� delta �Ȥ���,����ν��ޤ�Ȥ��Ʒ׻�����.i ���ܤζ�֤���������Ѥ�
(f (a+(i-1)delta)+f (a+i*delta))*delta/2
 *)
let rec sigma f n =
    if n = 0.0 then 0.0 else f n +. sigma f (n -. 1.0)
;;

let integral f a b = (* ��ʬ *)
  let loop n = (* n��롼�� *)
    let delta = (b -. a) /. n in 
    let trap = (* �����ʬ *)
      (fun i ->
       (f(a +. (i -. 1.0) *. delta) +. f(a +. i *. delta)) *. delta /. 2.0)
    in sigma trap n
  in loop 100.0
;;

let pi = acos(-1.) in integral sin 0.0 pi;;

(* ��delta�� �����ޤ�i���ܤ�׻�����ؿ���trap���Ѱդ��Ƥ����ơ�sigma��loop�����Ƥ��롣 *)

(*
integral (fun x -> x) 0. 1. =  0.5
 *)

(* ñ�ʤ�ư���ǧ *)
