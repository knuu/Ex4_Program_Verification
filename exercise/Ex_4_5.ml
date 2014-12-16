(** Ex 4.5 *)
let rec repeat f n x = (* f を n 回 x に適用する *)
  if n > 0 then repeat f (n - 1) (f x) else x;;

let fib n = 
  let (fibn, _) = 
    repeat (fun (x, y) -> (x + y, x)) n (0, 1)
  in fibn;;

(* テスト *)

let rec fib_pair n =
  if n = 1 then (0, 1)
  else
    let (prev, curr) = fib_pair (n - 1) in (curr, curr + prev)

;;

fib_pair 15 = (fib 14, fib 15);;


  

