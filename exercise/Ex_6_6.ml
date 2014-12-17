(**
二分木の左右を反転させた木を返す関数 reflect を定義せよ.
    # reflect comptree;;
    - : int tree =
    Br (1, Br (3, Br (7, Lf, Lf), Br (6, Lf, Lf)),
     Br (2, Br (5, Lf, Lf), Br (4, Lf, Lf)))
また,任意の二分木 t に対して成立する,以下の方程式を完成させよ.
preorder(reflect(t)) = ?
inorder(reflect(t)) = ?
postorder(reflect(t)) = ?
 *)

type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

let rec reflect = function
  | Lf -> Lf
  | Br(x, right, left) -> Br(x, reflect left, reflect right)
;;


(*
listを反転する関数をrevとすると
preorder(reflect(t)) = rev(postorder(t))
inorder(reflect(t)) = rev(inorder(t))
postorder(reflect(t)) = rev(preorder(t))
 *)

(* ********** *)
let comptree3 = Br(1, Br(2, Br(4, Lf, Lf),
                            Br(5, Lf, Lf)),
                      Br(3, Br(6, Lf, Lf),
                            Br(7, Lf, Lf)));;

let rev_comptree3 = (Br (1, Br (3, Br (7, Lf, Lf), 
			           Br (6, Lf, Lf)), 
			    Br (2, Br (5, Lf, Lf), 
				   Br (4, Lf, Lf))))
;;

let rec rev = function
    [] -> []
  | x :: rest -> (rev rest) @ [x];;

let rec preorder = function
    Lf -> []
  | Br (x, left, right) -> x :: (preorder left) @ (preorder right);;

let rec inorder = function
    Lf -> []
  | Br (x, left, right) -> (inorder left) @ (x :: inorder right);;

let rec postorder = function
          Lf -> []
        | Br (x, left, right) -> (postorder left) @ (postorder right) @ [x];;
