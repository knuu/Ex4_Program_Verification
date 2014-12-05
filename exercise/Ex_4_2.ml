(** Ex 4.2 *)
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
