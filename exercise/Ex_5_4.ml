(**
Exercise 5.4 f, g ��Ŭ���ʷ��δؿ��Ȥ���.map f (map g l) �� map ����٤������Ѥ��ʤ�Ʊ ��̣�μ��˽񤭴�����.map (fun x -> ...) l �� ... ��ʬ��?
 *)
(* 
map (fun x -> f (g x)) l 
�Ȥ�����ɤ���
 *)

let rec map f = function 
  | [] -> []
  | h :: t -> (f h) :: (map f t)
;;
let f x = x + 1;;
let g x = x * 2;;

map (fun x -> f (g x)) [0;1;2;3;4;5;6;7;8;9;10] = map f (map g [0;1;2;3;4;5;6;7;8;9;10]);;
