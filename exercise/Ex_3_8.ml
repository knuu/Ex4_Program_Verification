(** Ex 3.8 *)
let rec powi(x, n, res) = (* ȿ�����߾� *)
  if n = 0 then res
  else powi(x, n - 1, x *. res)
;;

(* �ƤӽФ���: �ʲ��Τ褦��pow���������3.7��Ʊ���褦�˰�����2���Ϥ� *)
let pow(x, n) = powi(x, n, 1.0);;
