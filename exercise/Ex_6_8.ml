(**
Exercise 6.8 1, 2, 3, 4 からなる可能な二分探索木の形を列挙し,それぞれの形を作るためには空の木から始めて,どの順番で要素を add していけばよいか示せ.
 *)

type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

let rec mem t x =
  match t with
    Lf -> false
  | Br (y, left, right) ->
     if x = y then true
     else if x < y then mem left x else mem right x
;;

let rec add t x =
  match t with
    Lf -> Br (x, Lf, Lf)
  | (Br (y, left, right) as whole) ->
     if x = y then whole
     else if x < y then Br(y, add left x, right) else Br(y, left, add right x)
;;
  
let bst1 = Br (1, Lf, Br (2, Lf, Br (3, Lf, Br (4, Lf, Lf))));;
let bst2 = Br (1, Lf, Br (2, Lf, Br (4, Br (3, Lf, Lf), Lf)));;
let bst3 = Br (1, Lf, Br (3, Br (2, Lf, Lf), Br (4, Lf, Lf)));;
let bst4 = Br (1, Lf, Br (4, Br (2, Lf, Br (3, Lf, Lf)), Lf));;
let bst5 = Br (1, Lf, Br (4, Br (3, Br (2, Lf, Lf), Lf), Lf));;
let bst6 = Br (2, Br (1, Lf, Lf), Br (3, Lf, Br (4, Lf, Lf)));;
let bst7 = Br (2, Br (1, Lf, Lf), Br (4, Br (3, Lf, Lf), Lf));;
let bst8 = Br (3, Br (1, Lf, Br (2, Lf, Lf)), Br (4, Lf, Lf));;
let bst9 = Br (3, Br (2, Br (1, Lf, Lf), Lf), Br (4, Lf, Lf));;
let bst10 = Br (4, Br (1, Lf, Br (2, Lf, Br (3, Lf, Lf))), Lf);;
let bst11 = Br (4, Br (1, Lf, Br (3, Br (2, Lf, Lf), Lf)), Lf);;
let bst12 = Br (4, Br (2, Br (1, Lf, Lf), Br (3, Lf, Lf)), Lf);;
let bst13 = Br (4, Br (3, Br (1, Lf, Br (2, Lf, Lf)), Lf), Lf);;
let bst14 = Br (4, Br (3, Br (2, Br (1, Lf, Lf), Lf), Lf), Lf);;

let bst1' = let a = add Lf 1 in let b = add a 2 in let c = add b 3 in add c 4;;
let bst2' = let a = add Lf 1 in let b = add a 2 in let c = add b 4 in add c 3;;
let bst3' = let a = add Lf 1 in let b = add a 3 in let c = add b 2 in add c 4;;
let bst4' = let a = add Lf 1 in let b = add a 3 in let c = add b 4 in add c 2;;
let bst5' = let a = add Lf 1 in let b = add a 4 in let c = add b 2 in add c 3;;
let bst6' = let a = add Lf 1 in let b = add a 4 in let c = add b 3 in add c 2;;
let bst7' = let a = add Lf 2 in let b = add a 1 in let c = add b 3 in add c 4;;
let bst8' = let a = add Lf 2 in let b = add a 1 in let c = add b 4 in add c 3;;
let bst9' = let a = add Lf 2 in let b = add a 3 in let c = add b 1 in add c 4;;
let bst10' = let a = add Lf 2 in let b = add a 3 in let c = add b 4 in add c 1;;
let bst11' = let a = add Lf 2 in let b = add a 4 in let c = add b 1 in add c 3;;
let bst12' = let a = add Lf 2 in let b = add a 4 in let c = add b 3 in add c 1;;
let bst13' = let a = add Lf 3 in let b = add a 1 in let c = add b 2 in add c 4;;
let bst14' = let a = add Lf 3 in let b = add a 1 in let c = add b 4 in add c 2;;
let bst15' = let a = add Lf 3 in let b = add a 2 in let c = add b 1 in add c 4;;
let bst16' = let a = add Lf 3 in let b = add a 2 in let c = add b 4 in add c 1;;
let bst17' = let a = add Lf 3 in let b = add a 4 in let c = add b 1 in add c 2;;
let bst18' = let a = add Lf 3 in let b = add a 4 in let c = add b 2 in add c 1;;
let bst19' = let a = add Lf 4 in let b = add a 1 in let c = add b 2 in add c 3;;
let bst20' = let a = add Lf 4 in let b = add a 1 in let c = add b 3 in add c 2;;
let bst21' = let a = add Lf 4 in let b = add a 2 in let c = add b 1 in add c 3;;
let bst22' = let a = add Lf 4 in let b = add a 2 in let c = add b 3 in add c 1;;
let bst23' = let a = add Lf 4 in let b = add a 3 in let c = add b 1 in add c 2;;
let bst24' = let a = add Lf 4 in let b = add a 3 in let c = add b 2 in add c 1;;

(* 合計14通り。作りかた24通りすべてを確かめればわかる。 *)
(*
bst1' = bst1;;
bst2' = bst2;;
bst3' = bst3;;
bst4' = bst3;;
bst5' = bst4;;
bst6' = bst5;;
bst7' = bst6;;
bst8' = bst7;;
bst9' = bst6;;
bst10' = bst6;;
bst11' = bst7;;
bst12' = bst7;;
bst13' = bst8;;
bst14' = bst8;;
bst15' = bst9;;
bst16' = bst9;;
bst17' = bst8;;
bst18' = bst9;;
bst19' = bst10;;
bst20' = bst11;;
bst21' = bst12;;
bst22' = bst12;;
bst23' = bst13;;
bst24' = bst14;;
 *)
(* 全通り確認 *)
