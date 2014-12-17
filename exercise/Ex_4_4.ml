(** Ex 4.4 *)
let curry f x y = f (x, y);;

let uncurry f (x, y) = f x y
;;
