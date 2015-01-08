(**
Exercise 3.11 �ʲ��δؿ����������.
1. Euclid �θ߽�ˡ���������κ������������ؿ� gcd.
2. �ƥ����ȤκƵ�Ū������� (m n) �����ؿ� comb.
3. �����Ƶ�Ū�ؿ���Ȥäƥե��ܥʥå�����׻����� fib_iter.(fib_pair �򸵤ˤ���Ȥ褤.)
4. Ϳ����줿ʸ����Τʤ��� ASCII �����ɤ��Ǥ��礭��ʸ�����֤� max_ascii �ؿ�.ʸ���� ��ʸ�����Ф���ˡ�� 2.2.5 ��򻲾ȤΤ���.(��������ϰտ�Ū�ˡ֤ʤˤ���­��ʤ��פ褦�����ꤷ�Ƥ���ޤ�.�ߤ�����ǽ���ؿ�������Хޥ˥奢���Ĵ�٤���,�ץ�����ǹ��פ��Ƥ�������.)
 *)

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
  
(* 1,2������̤�
3�Ϥ褯����fib�������Ƶ�(SICP�Ǥ�ä�)
4�Ϻǽ��ʸ����2���ܤ�ʸ������Ӥ��ơ�ASCII�����ɤ��礭���ä�����ʸ�� + 3���ܰʹߤ�ʸ�����Ƥ�max_ascii�ؿ���Ĵ�١�ʸ�����Ĺ����1�ˤʤ�ޤǤ���򷫤��֤��Ƥ��롣
 *)

(*
gcd(4885, 6839) = 977;;
comb(6, 3) = 20;;
comb(25, 12) = comb(25, 13);;
fib_iter 10 = 55;;
let rec fib n = if n = 1 || n = 2 then 1 else fib (n - 1) + fib (n - 2) in
fib_iter 10 = fib 10;;
max_ascii "String" = 't';;

 *)

(*
1,2,4,6���ܤ�ñ�ʤ�ư���ǧ
3���ܤ�(n m) = (n (n - m)) �Ȥ����������������Ƥ��뤫�Υƥ���
5���ܤϺƵ�Ū������̤��fib��fib_iter�ν��Ϥ��������γ�ǧ
 *)
