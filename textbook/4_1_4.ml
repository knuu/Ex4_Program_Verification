(** 4.1.4 Newton-Raphason法 *)
let deriv f = (* 微分係数 *)
  let dx = 0.1e-10
  in fun x -> (f(x +. dx) -. f(x)) /. dx
;;

let fixpoint f init = (* 不動点 *)
  let threshold = 0.1e-10 in
  let rec loop x =
    let next = f x in
    if abs_float (x -. next) < threshold then x
    else loop next
  in loop init
;;

let newton_transform f = (* ニュートン法の漸化式 *)
  fun x -> x -. f(x) /. (deriv f x)
;;

let newton_method f guess = (* ニュートン法 *)
  fixpoint (newton_transform f) guess
;;

let square_root x = (* 平方根 *)
  newton_method (fun y -> y *. y -. x) 1.0
;;

  
