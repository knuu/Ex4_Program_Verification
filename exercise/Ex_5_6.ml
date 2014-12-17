(**
Exercise 5.6 quick �ؿ��� @ ��Ȥ�ʤ��褦�˽񤭴�����. quicker ��̤�����ȤΥꥹ�� l ��, sorted �Ȥ��������ȺѤǤ������ǤκǾ��ͤ� l �����Ǥκ����ͤ���礭���褦�ʥꥹ�Ȥ�������. �������������.
    let rec quicker l sorted = ...
 *)
let rec quicker l sorted =
  match l with
  | [] -> sorted
  | [x] -> x :: sorted
  | x :: xs -> 
     let rec partition left right = function
       | [] -> quicker left (x :: (quicker right sorted))
       | y :: ys -> if  x < y then partition left (y :: right) ys
		    else partition (y :: left) right ys
     in partition [] [] xs
;;

(* ************ *)

let rec quick = function
  | [] -> []
  | [x] -> [x]
  | x :: xs ->
     let rec partition left right = function
       | [] -> (quick left) @ (x :: quick right)
       | y :: ys -> if x < y then partition left (y :: right) ys
		    else partition (y :: left) right ys
     in partition [] [] xs;;

let rec insert (x : float) = function
    [] -> [x]
  | (y :: rest) as l -> if x < y then x :: l else y :: (insert x rest)
;;
let rec insertion_sort = function
    [] -> []
  | x :: rest -> insert x (insertion_sort rest)
;;

let nextrand seed =
  let a = 16807.0 and m = 2147483647.0 in
  let t = a *. seed
  in t -. m *. floor (t /. m)
;;
let rec randlist n seed tail =
  if n = 0 then (seed, tail)
  else randlist (n - 1) (nextrand seed) (seed::tail)
;;
