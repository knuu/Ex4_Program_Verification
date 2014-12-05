(** Ex 3.7(2) *)
let rec pow(x, n) = (* �߾� *)
  if n = 0 then 1.0
  else 
    if n mod 2 = 0 then pow (x *. x, n / 2)
    else x *. pow(x *. x, (n - 1) / 2) 
;;
