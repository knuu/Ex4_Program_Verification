(** 
Exercise 6.2 nat 型の値をそれが表現する int に変換する関数 int_of_nat, nat 上の掛け算を行う関数 mul,nat 上の引き算を行う関数 (ただし 0 - n = 0) monus (モーナス) を定義せよ.(mul, monus は *, - などの助けを借りず,nat 型の値から「直接」計算するようにせよ.)
 *)
type nat = Zero | OneMoreThan of nat;;

let rec add m n =
  match m with Zero -> n | OneMoreThan m' -> OneMoreThan (add m' n)
;;

let rec int_of_nat n = 
  match n with
  | Zero -> 0
  | OneMoreThan n' -> succ (int_of_nat n')
;;

let rec mul m n = 
  match m with
  | Zero -> Zero
  | OneMoreThan m' -> add n (mul m' n)
;;

let rec monus m n =
  match m with
  | Zero -> Zero
  | OneMoreThan m' -> match n with
		      | Zero -> m
		      | OneMoreThan n' -> monus m' n'
;;

(* Zeroかそうでないかで場合分けして再帰的に計算する *)

let zero = Zero
let one = OneMoreThan zero
let two = OneMoreThan one
let three = OneMoreThan two;;
(*
int_of_nat(mul two three) = 6;;
int_of_nat(monus three one) = 2;;
 *)

(* 単なる動作確認 *)

