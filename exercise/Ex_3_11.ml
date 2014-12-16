(**
Exercise 3.11 以下の関数を定義せよ.
1. Euclid の互除法で二整数の最大公約数を求める関数 gcd.
2. テキストの再帰的な定義で (m n) を求める関数 comb.
3. 末尾再帰的関数を使ってフィボナッチ数を計算する fib_iter.(fib_pair を元にするとよい.)
4. 与えられた文字列のなかで ASCII コードが最も大きい文字を返す max_ascii 関数.文字列か ら文字を取出す方法は 2.2.5 節を参照のこと.(この問題は意図的に「なにかが足りない」ように設定してあります.欲しい機能・関数があればマニュアルを調べたり,プログラム上で工夫してください.)
 *)

(* 1 *)
let gcd(m, n) = (* 最大公約数 *)
  let rec eu(m, n) =
    if m = 0 then n else eu(n mod m, m)
  in if m > n then eu(n, m) else eu(m, n)
;;

(* 2 *)
let rec comb(n, m) = (* 組み合わせ *)
  if m = 0 || m = n then 1
  else comb(n - 1, m) + comb(n - 1, m - 1)
;;

(* 3 *)
let fib_iter n = (* 末尾再帰でFib *)
  let rec fibi(m, res, c) =
    if c <= 1 then res
    else fibi(m + res, m, c - 1)
  in fibi(1, 1, n)
;;

(* 4 *)
let max_ascii s = (* ASCIIコード最大の文字を求める *)
  let str_tl s = (* 文字列におけるtl *)
    if String.length s = 1 then "" 
    else String.sub s 1 (String.length s - 1)
  in let rec max_asc(c, s) =
       if String.length s = 0 then c
       else 
	 if c < s.[0] then max_asc(s.[0], str_tl s)
	 else max_asc(c, str_tl s)
  in max_asc(s.[0], s)
;;
  
    
