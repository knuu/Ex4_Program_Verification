(** Ex 4.4 *)
let uncurry f (x, y) = (* 非カリー化 *)
  f x y
;;

(* テスト *)
(*
let curry f x y = f (x, y);;
let average (x, y) = (x +. y) /. 2.0;;
let curried_avg = curry average;;

average (4.0, 5.3);;
curried_avg 4.0 5.3;;

let avg = uncurry curried_avg in avg (4.0, 5.3);;

(uncurry (curry average)) (4.0, 5.3) = average(4.0, 5.3);;
 *)
