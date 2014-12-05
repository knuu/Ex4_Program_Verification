(** Ex 3.11 *)
(* 1 *)
let gcd(m, n) = (* �������� *)
  let rec eu(m, n) =
    if m = 0 then n else eu(n mod m, m)
  in if m > n then eu(n, m) else eu(m, n)
;;

(* 2 *)
let rec comb(n, m) = (* �Ȥ߹�碌 *)
  if m = 0 || m = n then 1
  else comb(n - 1, m) + comb(n - 1, m - 1)
;;

(* 3 *)
let fib_iter n = (* �����Ƶ���Fib *)
  let rec fibi(m, res, c) =
    if c <= 1 then res
    else fibi(m + res, m, c - 1)
  in fibi(1, 1, n)
;;

(* 4 *)
let max_ascii s = (* ASCII�����ɺ����ʸ������� *)
  let str_tl s = (* ʸ����ˤ�����tl *)
    if String.length s = 1 then "" 
    else String.sub s 1 (String.length s - 1)
  in let rec max_asc(c, s) =
       if String.length s = 0 then c
       else 
	 if c < s.[0] then max_asc(s.[0], str_tl s)
	 else max_asc(c, str_tl s)
  in max_asc(s.[0], s)
;;
  
    
