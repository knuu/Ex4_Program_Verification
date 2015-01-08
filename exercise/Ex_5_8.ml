(**
Exercise 5.8 map の定義は末尾再帰的ではないため,入力リストの長さが長くなるとそれに比例した量のメモリ (スタック) が必要になる.map と同機能で,必要なメモリ量が定数オーダーである map2 を定義せよ.(ヒント: 末尾再帰的 (iterative) な関数を使う.)
 *)
let map2 f l = 
  let rec map_r f l f_l = 
    match l with
    | [] -> f_l
    | h :: t -> map_r f t (f_l @ [f h])
  in map_r f l []
;;

(* 末尾再帰化しただけ *)
(*
Ex_5_4.map (fun x -> x * 2) [4; 91; 0; -34] = map2 (fun x -> x * 2) [4; 91; 0; -34];;
 *)
(* 動作確認と末尾再帰でないmapとの等価性の確認 *)


