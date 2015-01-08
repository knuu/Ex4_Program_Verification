(**
Exercise 5.7 与えられた自然数 r に対して x^2 + y^2 = r であるような,(x, y) (ただし x >= y >= 0) の組を全てリストとして列挙する関数 squares r を定義せよ.(検算用資料: r = 48612265 の時 32 個の解があるそうです.)
 *)
let squares r = 
  let rec sq x =
    let rec y = int_of_float(sqrt(float_of_int(r - x * x)))
    in if x * x + x * x > r then [] 
       else if x * x + y * y = r then (x, y) :: sq (x + 1)
       else sq (x + 1)
  in sq 1
;;

(* x = 1から順番に、x * x + y * y = r を満たすペアを探している *)


let rec length = function | [] -> 0 | _ :: t -> succ(length t)
(*
length (squares 48612265)) = 32;;
 *)

(* 単なる動作確認 *)
