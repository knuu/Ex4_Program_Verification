(** 
Exercise 6.10 以下で定義される ('a, 'b) sum 型は,「α 型の値もしくは β 型の値」という和集合
的なデータの構成を示す型である.
    # type ('a, 'b) sum = Left of 'a | Right of 'b;;
    type ('a, 'b) sum = Left of 'a | Right of 'b
    # let float_of_int_or_float = function
          Left i -> float_of_int i
        | Right f -> f;;
    val float_of_int_or_float : (int, float) sum -> float = <fun>
    # float_of_int_or_float (Right 3.14);;
    - : float = 3.14
    # float_of_int_or_float (Left 2);;
- : float = 2.
これを踏まえて,次の型をもつ関数を定義せよ.
1. 'a * ('b, 'c) sum -> ('a * 'b, 'a * 'c) sum
2. ('a, 'b) sum * ('c, 'd) sum -> (('a * 'c, 'b * 'd) sum, ('a * 'd, 'b * 'c) sum) sum 3. ('a -> 'b) * ('c -> 'b) -> ('a, 'c) sum -> 'b
4. (('a, 'b) sum -> 'c) -> ('a -> 'c) * ('b -> 'c)
5. ('a -> 'b, 'a -> 'c) sum -> ('a -> ('b,'c) sum)
 *)

type ('a, 'b) sum = Left of 'a | Right of 'b;;

(* 1 *)
let f1 (x, y) =
  match y with
  | Left y1 -> Left (x, y1)
  | Right y2 -> Right (x, y2)
;;

(* 2 *)
let f2 (x, y) =
  match (x, y) with
  | (Left x1, Left y1) -> Left (Left (x1, y1))
  | (Left x1, Right y2) -> Right (Left (x1, y2))
  | (Right x2, Left y1) -> Right (Right (x2, y1))
  | (Right x2, Right y2) -> Left (Right (x2, y2))
;;

(* 3 *)
let f3 (f, g) x =
  match x with
  | Left x1 -> f x1
  | Right x2 -> g x2
;;

(* 4 *)


(* 5 *)
let f5 = function
  | Left f -> (fun x -> Right (f x))
  | Right g -> (fun x -> Left (g x))
;;

