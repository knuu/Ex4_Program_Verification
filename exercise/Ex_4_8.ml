(**
Exercise 4.8 double double f x �� f (f (f (f x))) �Ȥ���Ư����ͳ�������Ʊ�ͤˤ�����������
 *)

let double f x = f (f x);;
(*
double double f x
=> ((double double) f) x
=> (double (double f)) x 
=> (double f) ((double f) x)
=> (double f) (f (f x))
=> f (f (f (f x)))
 *)








