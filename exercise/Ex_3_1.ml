(** Ex 3.1 *)
(* 1: x+2 �� x ��ľ���� x=3 ��ؤ��� x*x �� x ��ľ���� x=x+2(=5) ��ؤ�����ä�ɾ����̤�25 *)
(* 2: y=x �� x �� x=y+2 �� y ��in�����Ϥ��줾�� x=2 �� y=3 ��ؤ���x*y �� x �� x=y+2(=5) ��ؤ��� y �� y=x(=2) ��ؤ����Ǹ�� y ��in������ y=3 ��ؤ�����ä�ɾ����̤�13 *)
(* 3: y=x �� x �� x=2 ��ؤ��� z=y+2 �� y �� y=x(=2) ��ؤ��� x*y*z ��x,y,z �Ϥ��줾�� x=2,y=x(=2),z=y+2(=4) ��ؤ�����ä�ɾ����̤�16 *)

let x = 1 in let x = 3 in let x = x + 2 in x * x;;

let x = 2 and y = 3 in (let y = x and x = y + 2 in x * y) + y;;

let x = 2 in let y = 3 in let y = x in let z = y + 2 in x * y * z ;;
