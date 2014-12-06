(** Ex 5.5 *)
let rec fold_right f l e =
  match l with
    [] -> e
  | x :: rest -> f x (fold_right f rest e)
;;

let rec map f = function
    [] -> []
  | x :: rest -> f x :: map f rest
;;

let forall p l = fold_right (&&) (map p l) true
;;
let exists p l = fold_right (||) (map p l) false
;;

(* Test *)
forall (fun c -> 'z' > c) ['A'; ' '; '+'] = true;;
exists (fun x -> (x mod 7) = 0) [23; -98; 19; 53] = true;;
