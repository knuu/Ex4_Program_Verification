(**
Exercise 6.4 ���� n �����ƤΥΡ��ɤΥ�٥뤬 x �Ǥ���褦�ʴ�����ʬ�ڤ���������ؿ� comptree x n ���������.
 *)

type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

let rec comptree x n = 
  if n = 1 then Br(x, Lf, Lf)
  else Br(x, comptree x (n - 1), comptree x (n - 1))
;;

(* Test *)
let comptree3 = Br(1, Br(1, Br(1, Lf, Lf),
			    Br(1, Lf, Lf)),
                      Br(1, Br(1, Lf, Lf),
			    Br(1, Lf, Lf)));;

let rec depth = function
    Lf -> 0
  | Br (_, left, right) -> 1 + max (depth left) (depth right);;

comptree 1 3 = comptree3;;
depth(comptree 1 3) = 3;;


