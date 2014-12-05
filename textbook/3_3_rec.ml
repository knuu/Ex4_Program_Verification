(** ���ʽ� 3.3 �Ƶ��ؿ� *)
let rec fact n = (* factorial of positive n *)
  if n <= 1 then 1 else fact (n-1) * n;;

#trace fact;;
#untrace fact;;

let fact2 n = (* �����Ƶ���fact *)
  let rec facti (n, res) =
    if n <= 1 then res else facti (n-1, res * n)
  in facti (n, 1);;

let rec fib n = (* �ե��ܥʥå����� *)
  if n <= 0 then 0
  else if n = 1 then 1
  else fib (n-2) + fib (n-1)
;; 

let fib2 n = (* �����Ƶ���fib *)
  let rec fibi n =
    if n <= 0 then (0, 0)
    else if n = 1 then (0, 1)
    else let (prev, curr) = fibi (n-1) in (curr, curr + prev)
  in let (_, res) = fibi n in res
;;

let rec even n = (* �����������Ƚ�� *)
  if n = 0 then true else odd(n-1)
and odd n =
  if n = 0 then false else even(n-1)
;;

let leibniz_pi n = (* pi/4 *)
  let rec pos n = (* �� *)
    neg (n-1) +. 1.0 /. (float_of_int (4 * n + 1))
  and neg n = (* �� *)
    if n < 0 then 0.0
    else pos n -. 1.0 /. (float_of_int (4 * n + 3))
  in if even n then 4.0 *. pos n else 4.0 *. neg n
;;
