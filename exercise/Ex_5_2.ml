(** Ex 5.2 *)
let hd (x::rest) = x;;
let tl (x::rest) = rest;;
let null = function [] -> true | _ -> false;;
