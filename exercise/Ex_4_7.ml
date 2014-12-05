(** Ex 4.7 *)
(*
s k k 1
-> k 1 (k 1)
-> 1
 *)

let k x y = x;;
let s x y z = x z (y z);;

(k (s k k)) 1 2;;


