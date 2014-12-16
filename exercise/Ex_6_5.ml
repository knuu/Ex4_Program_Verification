(** 
Exercise 6.5 preord と同様な方法で,通りがけ順,帰りがけ順に列挙する関数 inord, postord を 定義せよ.
 *)

type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

let rec inord t l = 
  match t with
  | Lf -> l
  | Br(x, left, right) -> inord left (x :: (inord right l))
;;

let rec postord t l = 
  match t with
  | Lf -> l
  | Br(x, left, right) -> postord left (postord right (x :: l))
;;

(* Test *)

let comptree3 = Br(1, Br(2, Br(4, Lf, Lf),
                           Br(5, Lf, Lf)),
                     Br(3, Br(6, Lf, Lf),
			   Br(7, Lf, Lf)));;

inord comptree3 [] = [4; 2; 5; 1; 6; 3; 7];;
postord comptree3 [] = [4; 5; 2; 6; 7; 3; 1];;



