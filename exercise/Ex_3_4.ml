(** Ex 3.4 *)
(* 行列はm行n列の行列なら、m行目を要素としてもつm個のペアを要素とするペアとして表現した。つまり、
[a_11 … a_1n]
[…   …   …]
[a_m1 … a_mn]
なら、((a_11, …, a_1n), …, (a_m1, …, a_mn)) というペアをとなる。 *)

let prodMatVec pair = (* 1次変換 *)
  let (((a11, a12), (a21,a22)), (x, y)) = pair in
  (a11 *. x +. a12 *. y, a21 *. x +. a22 *. y);;
