(** 
Exercise 4.2 練習問題 3.7 の pow 関数をカリー化して定義せよ. 次に第一引数が指数になるよう (pow n x) に定義し,3 乗する関数 cube を部分適用で定義せよ.指数が第二引数であるように定義されている場合 (pow x n),cube を pow から定義するにはどうすればよいか?
 *)
let rec pow1 x n = (* カリー化された累乗1 *)
  if n = 0 then 1.0 else x *. pow1 x (n-1)
;;

let rec pow2 n x = (* カリー化された累乗2 *)
  if n = 0 then 1.0 else x *. pow2 (n-1) x
;;
  
let cube1 = (* pow2からcubeを定義 *)
  pow2 3;;

let cube2 = (* pow1からcubeを定義 *)
  (fun x -> pow1 x 3)
;;

(* テスト *)
(*
cube1 3. = cube2 3.;;
 *)
