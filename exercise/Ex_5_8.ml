(** Ex 5.8 *)
let rec map f = function 
  | [] -> []
  | h :: t -> (f h) :: (map f t)
;;

let map2 f l = 
  let rec map_r f l f_l = 
    match l with
    | [] -> f_l
    | h :: t -> map_r f t (f_l @ [f h])
  in map_r f l []
;;

(* Test *)
map (fun x -> x * 2) [4; 91; 0; -34] = map2 (fun x -> x * 2) [4; 91; 0; -34];;
