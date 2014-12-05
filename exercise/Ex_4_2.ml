(** Ex 4.2 *)
let rec pow1 x n = (* ���꡼�����줿�߾�1 *)
  if n = 0 then 1.0 else x *. pow1 x (n-1)
;;

let rec pow2 n x = (* ���꡼�����줿�߾�2 *)
  if n = 0 then 1.0 else x *. pow2 (n-1) x
;;
  
let cube1 = (* pow2����cube����� *)
  pow2 3;;

let cube2 = (* pow1����cube����� *)
  (fun x -> pow1 x 3)
;;

(* �ƥ��� *)
(*
cube1 3. = cube2 3.;;
 *)
