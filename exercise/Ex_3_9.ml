(** 
Exercise 3.9 if 式は Objective Caml の関数で表現することはできない.以下の関数はそれを試みたものである.fact 4 の計算の評価ステップを考え,なぜうまく計算できないのか説明せよ.
# let cond (b, e1, e2) : int = if b then e1 else e2;;
  val cond : bool * int * int -> int = <fun>
# let rec fact n = cond ((n = 1), 1, n * fact (n-1));;
  val fact : int -> int = <fun>
# fact 4;;
  ???? 
 *)
(*
let cond (b, e1, e2) : int  = if b then e1 else e2;;
let rec fact n = cond ((n = 1), 1, n * fact (n - 1));;
 *)
(* ここでfact 4を計算すると、無限ループを起こす。 なぜならOCamlでは評価戦略として、資料p.29の値呼出しの戦略をとっているからである。これは、関数を適用するときにはまず引数を先に評価するというものであり、fact 4でみてみると、
fact 4 (* 引数は既に評価されているのでfactを展開 *)
-> cond ((4 = 1), 1, 4 * fact 3) (* 引数を評価 *)
-> cond (true, 1, 4 * cond((3 = 1), 1, fact 2)) 
-> …
というように、末尾のfactの引数が無限に小さくなっていってしまう。
よって無限ループを起こし、うまく計算出来ない。
 *)
