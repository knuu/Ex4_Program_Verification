(**
Exercise 5.8 map の定義は末尾再帰的ではないため,入力リストの長さが長くなるとそれに比例した量のメモリ (スタック) が必要になる.map と同機能で,必要なメモリ量が定数オーダーである map2 を定義せよ.(ヒント: 末尾再帰的 (iterative) な関数を使う.)
 *)
let rec map f = function 
  | [] -> []
  | h :: t -> (f h) :: (map f t)
;;

let map2 f l = 
  let rec map_r f l f_l = 
    match l with
    | [] -> f_l
    | h :: t -> map_r f t (f_l @ [f h])
  in map_r f l []
;;

