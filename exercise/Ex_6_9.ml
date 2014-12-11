(**
Exercise 6.9 関数 sift を定義し,⟨ 自分の学籍番号 + 3000⟩ 番目の素数を求めよ.
 *)

type 'a seq = Cons of 'a * (unit -> 'a seq);;
let rec from n = Cons (n, fun () -> from (n + 1));;
let head (Cons (x, _)) = x;;
let tail (Cons (_, f)) = f ();;
let rec take n s =
  if n = 0 then [] else head s :: take (n - 1) (tail s);;
let rec mapseq f (Cons (x, tail)) =
  Cons (f x, fun () -> mapseq f (tail ()));;

let rec sift n (Cons (x, f)) =
  if x mod n = 0 then sift n (Cons (head (f ()), fun () -> tail (f ())))
  else Cons (x, fun () -> sift n (f ()))
;;

let rec sieve (Cons (x, f)) = Cons (x, fun () -> sieve (sift x (f())));;
let primes = sieve (from 2);;
take 20 primes = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71];;

let rec nthseq n (Cons (x, f)) =
  if n = 1 then x else nthseq (n - 1) (f());;

nthseq 1000 primes = 7919;;

nthseq (1029240446 + 3000) primes;;