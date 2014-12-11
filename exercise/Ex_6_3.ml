(**
Exercise 6.3 上の monus 関数を変更して,0−n (n > 0) は None を返す nat -> nat -> nat option 型の関数 minus を定義せよ.
 *)

type nat = Zero | OneMoreThan of nat;;
type 'a option = None | Some of 'a;;

let rec monus m n = 
  match m with
  | Zero -> if n = Zero then Some Zero else None
  | OneMoreThan m' -> match n with
		      | Zero -> Some m
		      | OneMoreThan n' -> monus m' n'
;;

(* Test *)
let rec monus' m n =
  match m with
  | Zero -> Zero
  | OneMoreThan m' -> match n with
		      | Zero -> m
		      | OneMoreThan n' -> monus' m' n'
;;

let rec get_value op default =
  match op with
  | None -> default
  | Some n -> n
;;

let zero = Zero;;
let one = OneMoreThan zero;;
let two = OneMoreThan one;;
let three = OneMoreThan two;;

monus two three = None;;
get_value (monus three two) Zero = monus' three two;;



