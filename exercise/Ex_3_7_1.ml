(** Ex 3.7(1) *)
let rec pow(x, n) = (* Îß¾è *)
  if n = 0 then 1.0
  else x *. pow (x, n-1)
;;
