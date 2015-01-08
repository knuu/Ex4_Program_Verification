(**
Exercise 4.1 実数上の関数 f に対して integral [a,b] f(x)dx を計算する関数 integral f a b を定義せよ.
またこれを使って,integral [0,π] sin x dx を計算せよ
近似的な計算方法として,ここでは台形近似を説明するが他の方法でも良い.台形公式では b - a を n 分割した区間の長さを delta として,台形の集まりとして計算する.i 番目の区間の台形の面積は
(f (a+(i-1)delta)+f (a+i*delta))*delta/2
 *)
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

let pi = acos(-1.) in integral sin 0.0 pi;;

(* 幅deltaと シグマのi番目を計算する関数をtrapを用意しておいて、sigmaをloopさせている。 *)

(*
integral (fun x -> x) 0. 1. =  0.5
 *)

(* 単なる動作確認 *)
