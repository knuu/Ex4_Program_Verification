(** Ex 3.1 *)
(* 1: x+2 の x は直前の x=3 を指し、 x*x の x は直前の x=x+2(=5) を指す。よって評価結果は25 *)
(* 2: y=x の x と x=y+2 の y はinの前はそれぞれ x=2 と y=3 を指す。x*y の x は x=y+2(=5) を指し、 y は y=x(=2) を指す。最後の y はinの前の y=3 を指す。よって評価結果は13 *)
(* 3: y=x の x は x=2 を指し、 z=y+2 の y は y=x(=2) を指し、 x*y*z のx,y,z はそれぞれ x=2,y=x(=2),z=y+2(=4) を指す。よって評価結果は16 *)

let x = 1 in let x = 3 in let x = x + 2 in x * x;;

let x = 2 and y = 3 in (let y = x and x = y + 2 in x * y) + y;;

let x = 2 in let y = 3 in let y = x in let z = y + 2 in x * y * z ;;
