(**
Exercise 5.4 f, g を適当な型の関数とする.map f (map g l) を map を一度しか使用しない同 意味の式に書き換えよ.map (fun x -> ...) l の ... 部分は?
 *)
(* 
map (fun x -> f (g x)) l 
とすれば良い。
 *)

let rec map f = function 
  | [] -> []
  | h :: t -> (f h) :: (map f t)
;;
let f x = x + 1;;
let g x = x * 2;;

map (fun x -> f (g x)) [0;1;2;3;4;5;6;7;8;9;10] = map f (map g [0;1;2;3;4;5;6;7;8;9;10]);;
