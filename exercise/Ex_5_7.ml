(**
Exercise 5.7 Ϳ����줿������ r ���Ф��� x^2 + y^2 = r �Ǥ���褦��,(x, y) (������ x >= y >= 0) ���Ȥ����ƥꥹ�ȤȤ�����󤹤�ؿ� squares r ���������.(�����ѻ���: r = 48612265 �λ� 32 �Ĥβ򤬤��뤽���Ǥ�.)
 *)
let squares r = 
  let rec sq x =
    let rec y = int_of_float(sqrt(float_of_int(r - x * x)))
    in if x * x + x * x > r then [] 
       else if x * x + y * y = r then (x, y) :: sq (x + 1)
       else sq (x + 1)
  in sq 1
;;

(* x = 1������֤ˡ�x * x + y * y = r ���������ڥ���õ���Ƥ��� *)


let rec length = function | [] -> 0 | _ :: t -> succ(length t)
(*
length (squares 48612265)) = 32;;
 *)

(* ñ�ʤ�ư���ǧ *)
