(** 教科書 3.3 再帰関数 *)
let rec fact n = (* factorial of positive n *)
  if n <= 1 then 1 else fact (n-1) * n;;

#trace fact;;
#untrace fact;;

let fact2 n = (* 末尾再帰版fact *)
  let rec facti (n, res) =
    if n <= 1 then res else facti (n-1, res * n)
  in facti (n, 1);;

let rec fib n = (* フィボナッチ数列 *)
  if n <= 0 then 0
  else if n = 1 then 1
  else fib (n-2) + fib (n-1)
;; 

let fib2 n = (* 末尾再帰版fib *)
  let rec fibi n =
    if n <= 0 then (0, 0)
    else if n = 1 then (0, 1)
    else let (prev, curr) = fibi (n-1) in (curr, curr + prev)
  in let (_, res) = fibi n in res
;;

let rec even n = (* 偶数・奇数を判断 *)
  if n = 0 then true else odd(n-1)
and odd n =
  if n = 0 then false else even(n-1)
;;

let leibniz_pi n = (* pi/4 *)
  let rec pos n = (* 正 *)
    neg (n-1) +. 1.0 /. (float_of_int (4 * n + 1))
  and neg n = (* 負 *)
    if n < 0 then 0.0
    else pos n -. 1.0 /. (float_of_int (4 * n + 3))
  in if even n then 4.0 *. pos n else 4.0 *. neg n
;;
