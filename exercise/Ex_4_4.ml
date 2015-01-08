(** Ex 4.4 *)
let curry f x y = f (x, y);;

let uncurry f (x, y) = f x y
;;

(* 見たとおり(Coqでもやった通り)なので説明省略 *)
(*
let average (x, y) = (x +. y) /. 2. in
(uncurry (curry average)) (4.0, 5.3) = average(4.0, 5.3);;
 *)
(* 適当な関数を定義して、カリー化して非カリー化すればもとに戻ることの確認 *)
