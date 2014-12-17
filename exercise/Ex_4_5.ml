(**
Exercise 4.5 �ʲ��δؿ� repeat �� double, fourtimes �ʤɤ���̲�������Τ�,f �� n ��,x ��Ŭ�Ѥ���ؿ��Ǥ���.
    # let rec repeat f n x =
        if n > 0 then repeat f (n - 1) (f x) else x;;
    val repeat : ('a -> 'a) -> int -> 'a -> 'a = <fun>
�����Ȥä�,�ե��ܥʥå�����׻�����ؿ� fib ���������.�ʲ��� ... ����ʬ������.
    let fib n =
      let (fibn, _) = ...
      in fibn;;
 *)

let rec repeat f n x = (* f �� n �� x ��Ŭ�Ѥ��� *)
  if n > 0 then repeat f (n - 1) (f x) else x;;

let fib n = 
  let (fibn, _) = 
    repeat (fun (x, y) -> (x + y, x)) n (0, 1)
  in fibn;;

