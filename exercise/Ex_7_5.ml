(**
Exercise 7.5 ����ؿ� fact ����ΰ������Ф��� Invalid_argument ��ȯ��������褦�˲��ɤ���.
 *)

exception Invalid_argument;;

let rec fact n =
  if n < 0 then raise Invalid_argument
  else if n = 0 then 1 else n * fact (n - 1)
;;

