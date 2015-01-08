(**
Exercise 6.3 上の monus 関数を変更して,0-n (n > 0) は None を返す nat -> nat -> nat option 型の関数 minus を定義せよ.
 *)

type 'a option = None | Some of 'a;;

let rec monus m n = 
  match m with
  | Ex_6_2.Zero -> if n = Ex_6_2.Zero then Some Ex_6_2.Zero else None
  | Ex_6_2.OneMoreThan m' -> match n with
		      | Ex_6_2.Zero -> Some m
		      | Ex_6_2.OneMoreThan n' -> monus m' n'
;;

(* 返り値をoption型に書き換えただけ *)

let rec get_value op default =
  match op with
  | None -> default
  | Some n -> n
;;
(*
monus Ex_6_2.two Ex_6_2.three = None;;
get_value (monus Ex_6_2.three Ex_6_2.two) Ex_6_2.Zero = Ex_6_2.monus' Ex_6_2.three Ex_6_2.two;;
 *)

(* Noneになる場合のテストと そうでない場合のテスト *)
