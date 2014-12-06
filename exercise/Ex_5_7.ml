(** Ex 5.7 *)
let squares r = 
  let rec sq x =
    let rec y = int_of_float(sqrt(float_of_int(r - x * x)))
    in if x * x + x * x > r then [] 
       else if x * x + y * y = r then (x, y) :: sq (x + 1)
       else sq (x + 1)
  in sq 1
;;

(* Test *)
let rec length = function | [] -> 0 | _ :: t -> succ(length t);;
length(squares 48612265) = 32;;
