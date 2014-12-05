(** Ex 4.1 *)
let rec sigma f n =
    if n = 0.0 then 0.0 else f n +. sigma f (n -. 1.0)
;;

let integral f a b = (* 積分 *)
  let loop n = (* n回ループ *)
    let delta = (b -. a) /. n in 
    let trap = (* 台形部分 *)
      (fun i ->
       (f(a +. (i -. 1.0) *. delta) +. f(a +. i *. delta)) *. delta /. 2.0)
    in sigma trap n
  in loop 100.0
;;

(* テスト *)
(*
integral (fun x -> x) 0. 1.;;

let pi = 3.1415926535;;
integral sin 0.0 pi;;
 *)

