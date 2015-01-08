(**
Exercise 5.3 ���δؿ����������.
1. �������� n ���� 0 �ޤǤ������ι߽�ꥹ�Ȥ���������ؿ� downto0.

2. Ϳ����줿���������Υ��޿���ɽ�� (ʸ����) �����ؿ� roman.(I = 1, V = 5, X = 10, L = 50, C = 100, D = 500, M = 1000 �Ǥ���.) ������,roman �ϥ��޿��������������Ȥ��Ƽ�����뤳�Ȥˤ���.���޿��������,ñ�̤Ȥʤ���ȥ��޿���ɽ�����Ȥ��礭����Τ����¤٤��ꥹ�Ȥ�ɽ������.�㤨��
roman [(1000, "M"); (500, "D"); (100, "C"); (50, "L");
       (10, "X"); (5, "V"); (1, "I")] 1984
==> "MDCCCCLXXXIIII"
4, 9, 40, 90, 400, 900 �ʤɤ�ɽ���ˤ���դ���,
roman [(1000, "M"); (900, "CM"); (500, "D"); (400, "CD");
       (100, "C"); (90, "XC"); (50, "L"); (40, "XL");
       (10, "X"); (9, "IX"); (5, "V"); (4, "IV"); (1, "I")] 1984
==> "MCMLXXXIV"
�Ȥʤ�褦�ˤ���.

3. Ϳ����줿�ꥹ�ȤΥꥹ�Ȥ��Ф�,��¦�Υꥹ�Ȥ����Ǥ��¤٤��ꥹ�Ȥ��֤��ؿ� concat.
concat [[0; 3; 4]; [2]; [5; 0]; []] = [0; 3; 4; 2; 5; 0]

4. ��ĤΥꥹ��[a1; ...; an]��[b1; ...; bn]������Ȥ���,[(a1, b1); ...; (an, bn)] ���֤��ؿ� zip.(Ϳ����줿�ꥹ�Ȥ�Ĺ�����ۤʤ����Ĺ���ꥹ�Ȥ�;�ä���ʬ��ΤƤƤ褤.)

5. �ꥹ�Ȥ�,�ꥹ�Ȥ����Ǿ�νҸ� ( bool �����֤��ؿ�) p ��Ȥä�,p �����������Ƥ����ǤΥꥹ�Ȥ��֤��ؿ� filter.
# let positive x = (x > 0);;
val positive : int -> bool = <fun>
# filter positive [-9; 0; 2; 5; -3];;
- : int list = [2; 5]
# filter (fun l -> length l = 3) [[1; 2; 3]; [4; 5]; [6; 7; 8]; [9]];;
- : int list list = [[1; 2; 3]; [6; 7; 8]]

6. �ꥹ�Ȥ򽸹�Ȥߤʤ���,�ʲ��ν���黻�򤹤�ؿ����������.
(a) belong a s �� a �� s �����Ǥ��ɤ�����Ƚ�ꤹ��ؿ� belong. 
(b) intersect s1 s2 �� s1 �� s2 �ζ�����ʬ���֤��ؿ� intersect.
(c) union s1 s2 �� s1 �� s2 ���¤��֤��ؿ� union. 
(d) diff s1 s2 �� s1 �� s2 �κ����֤��ؿ� diff.
â��,����ʤΤ�,���ǤϽ�ʣ���Ƹ���ƤϤʤ�ʤ����Ȥ˵���Ĥ���.(�ؿ��ΰ����ˤϽ�ʣ ���Ƥʤ���Τ�Ϳ������Ȥ���������֤��Ƥ�褤.)
 *)
(* 1 *)
let rec downto0 n = (* descending order list *)
  if n = 0 then [0] else n :: downto0 (n - 1)
;;


(* 2 *)
let rec roman roman_list num = (* change arabic to roman *)
  match roman_list with
  | [] -> ""
  | (n, s) :: t -> if num > n then s ^ (roman roman_list (num - n))
		   else if num = n then s ^ (roman t (num - n))
		   else roman t num
;;

(* 3 *)
let rec concat = function
  | [] -> []
  | h :: t -> h @ (concat t)
;;

(* 4 *)
let rec zip l1 l2 = 
  match l1 with
  | [] -> []
  | h1 :: t1 -> match l2 with
		| [] -> []
		| h2 :: t2 -> (h1, h2) :: zip t1 t2
;;

(* 5 *)
let rec filter f = function
  | [] -> []
  | h :: t -> if (f h) then h :: (filter f t) else filter f t
;;

(* 6 *)
(* a *)
let rec belong a = function (* belong a with s ? *)
  | [] -> false 
  | h :: t -> if h = a then true else belong a t
;;
(* b *)
let rec intersect s1 s2 =
  match s1 with
  | [] -> []
  | h :: t -> if belong h s2 then h :: (intersect t s2)
	      else intersect t s2
;;
(* c *)
let rec union s1 s2 =
  match s1 with
  | [] -> s2
  | h :: t -> if belong h s2 then union t s2 else h :: (union t s2)
;;
(* d *)
let rec diff s1 s2 = 
  match s1 with 
  | [] -> []
  | h :: t -> if belong h s2 then diff t s2 else h :: (diff t s2)
;;

(*
downto: 0�ޤǤɤ�ɤ�����򤯤äĤ��Ƥ�������
roman: �礭������(ʸ��) ��Ȥ�������Ȥ����äơ���Ȥο�����0�ˤʤ�ޤǷ����֤�
concat: @�ǤĤʤ��Ǥ�������
zip: ����ɤ���ڥ��ˤ��Ƥ�������
filter: �Ƶ�Ū�η׻����Ƥ�������
belong: �Ƶ�Ū��Ĵ�٤Ƥ��äơ������true
intersect: s1�γ����Ǥ����뤫s2�ˤ��뤫��belong��Ĵ�٤ơ������Τ����֤�
diff: s1�����ǤΤ���s2�ˤʤ���Τ�belong��Ĵ�٤�
 *)

(*
downto0 10 = [10;9;8;7;6;5;4;3;2;1;0];;
roman [(1000, "M"); (500, "D"); (100, "C"); (50, "L"); (10, "X"); (5, "V"); (1, "I")] 1984 = "MDCCCCLXXXIIII";;
roman [(1000, "M"); (900, "CM"); (500, "D"); (400, "CD"); (100, "C"); (90, "XC"); (50, "L"); (40, "XL"); (10, "X"); (9, "IX"); (5, "V"); (4, "IV"); (1, "I")] 1984 = "MCMLXXXIV";;
concat [[0; 3; 4]; [2]; [5; 0]; []] = [0; 3; 4; 2; 5; 0];;
zip [1;2;3] [2;3;4] = [(1,2);(2,3);(3,4)];;
zip [1;1;1;1] [1;1] = [1;1] [1;1;1;1];;
zip [] [1] = zip [1] [];;
filter (fun x -> x > 0) [-9; 0; 2; 5; -3] = [2; 5];;
let rec length = function
    | [] -> 0 
    | _ :: t -> succ (length t) in
filter (fun l -> length l = 3) [[1; 2; 3]; [4; 5]; [6; 7; 8]; [9]] = [[1; 2; 3]; [6; 7; 8]];;
belong [3;4] [[1];[2;3];[3;4];[3;4;5]] = not (belong [] [[1];[2];[3]]);;
intersect [1;4;7;8;9;10] [4;7;1;0;3] = [1;4;7];;
union [1;2;3;4] [2;3;4;5] = [1;2;3;4;5];;
diff [1;2;3;4] [2;4;6;7] = [1;3];;
 *)

(* 
1,2,3,4,8,9,11,12,13���ܤ�ñ�ʤ�ư���ǧ
5,6���ܤ�zip�ΰ�����Ĺ����Ʊ�����Ȱۤʤ���γ�ǧ
7���ܤ�zip�ΰ��������ꥹ�ȤΤȤ��γ�ǧ
10���ܤ�ư���ǧ�������������ꥹ�ȤΤȤ���ư���ǧ
 *)
 
