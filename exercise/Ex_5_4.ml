(** Ex 5.4 *)
(* 
map (fun x -> f x && g x) l 
とすれば良い。
 *)

(* Test *)

let rec map f = function 
  | [] -> []
  | h :: t -> (f h) :: (map f t)
;;
let f x = x + 1;;
let g x = x * 2;;

map (fun x -> f (g x)) [0;1;2;3;4;5;6;7;8;9;10] = map f (map g [0;1;2;3;4;5;6;7;8;9;10]);;
