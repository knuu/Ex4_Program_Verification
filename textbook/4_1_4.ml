(** 4.1.4 Newton-Raphasonˡ *)
let deriv f = (* ��ʬ���� *)
  let dx = 0.1e-10
  in fun x -> (f(x +. dx) -. f(x)) /. dx
;;

let fixpoint f init = (* ��ư�� *)
  let threshold = 0.1e-10 in
  let rec loop x =
    let next = f x in
    if abs_float (x -. next) < threshold then x
    else loop next
  in loop init
;;

let newton_transform f = (* �˥塼�ȥ�ˡ�������� *)
  fun x -> x -. f(x) /. (deriv f x)
;;

let newton_method f guess = (* �˥塼�ȥ�ˡ *)
  fixpoint (newton_transform f) guess
;;

let square_root x = (* ʿ���� *)
  newton_method (fun y -> y *. y -. x) 1.0
;;

  
