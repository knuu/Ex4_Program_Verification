(** Ex 5.3 *)
(* 1 *)
let rec downto0 n = (* descending order list *)
  if n = 0 then [0] else n :: downto0 (n - 1)
;;


(* 2 *)
let rec roman roman_list num = (* change arabic to roman *)
  match roman_list with
  | [] -> ""
  | (n, s) :: t -> if num > n then s ^ (roman roman_list (num - n))
		   else if num = n then s ^ (roman t (num - n))
		   else roman t num
;;

(* 3 *)
let rec concat = function
  | [] -> []
  | h :: t -> h @ (concat t)
;;

(* 4 *)
let rec zip l1 l2 = 
  match l1 with
  | [] -> []
  | h1 :: t1 -> match l2 with
		| [] -> []
		| h2 :: t2 -> (h1, h2) :: zip t1 t2
;;

(* 5 *)
let rec filter f = function
  | [] -> []
  | h :: t -> if (f h) then h :: (filter f t) else filter f t
;;

(* 6: Set Operators 引数には重複した要素は与えられないと仮定する *)
(* a *)
let rec belong a = function (* belong a with s ? *)
  | [] -> false 
  | h :: t -> if h = a then true else belong a t
;;
(* b *)
let rec intersect s1 s2 =
  match s1 with
  | [] -> []
  | h :: t -> if belong h s2 then h :: (intersect t s2)
	      else intersect t s2
;;
(* c *)
let rec union s1 s2 =
  match s1 with
  | [] -> s2
  | h :: t -> if belong h s2 then union t s2 else h :: (union t s2)
;;
(* d *)
let rec diff s1 s2 = 
  match s1 with 
  | [] -> []
  | h :: t -> if belong h s2 then diff t s2 else h :: (diff t s2)
;;

(* Test *)

(* 1 *)
downto0 10 = [10;9;8;7;6;5;4;3;2;1;0];;

(* 2 *)
roman [(1000, "M"); (500, "D"); (100, "C"); (50, "L"); (10, "X"); (5, "V"); (1, "I")] 1984 = "MDCCCCLXXXIIII";;
roman [(1000, "M"); (900, "CM"); (500, "D"); (400, "CD"); (100, "C"); (90, "XC"); (50, "L"); (40, "XL"); (10, "X"); (9, "IX"); (5, "V"); (4, "IV"); (1, "I")] 1984 = "MCMLXXXIV";;

(* 3 *)
concat [[0; 3; 4]; [2]; [5; 0]; []] = [0; 3; 4; 2; 5; 0];;

(* 4 *)
zip [1;2;3] [2;3;4] = [(1,2);(2,3);(3,4)];;
zip [1;1;1;1] [1;1] = zip [1;1] [1;1;1;1];;
zip [] [1] = zip [1] [];; 

(* 5 *)
let rec length = function [] -> 0 | _ :: t -> succ (length t);;
filter (fun x -> x > 0) [-9; 0; 2; 5; -3] = [2; 5];;
filter (fun l -> length l = 3) [[1; 2; 3]; [4; 5]; [6; 7; 8]; [9]] = [[1; 2; 3]; [6; 7; 8]];;

(* 6 *)
belong [3;4] [[1];[2;3];[3;4];[3;4;5]] = not (belong [] [[1];[2];[3]]);;
intersect [1;4;7;8;9;10] [4;7;1;0;3] = [1;4;7];;
union [1;2;3;4] [2;3;4;5] = [1;2;3;4;5];;
diff [1;2;3;4] [2;4;6;7] = [1;3];;


