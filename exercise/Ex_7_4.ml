(**
Exercise 7.4 ���Ȥ�ȤäƳ���ؿ���������Ƥߤ褦. ...��ʬ������.
    let fact_imp n =
      let i = ref n and res = ref 1 in
        while (...) do
          ...;
          i := !i - 1
        done;
...;;
 *)

let fact_imp n =
  let i = ref n and res = ref 1 in
  while (!i > 0) do
    res := !res * !i;
    i := !i - 1
  done; 
  !res;;

(* Test *)
fact_imp 6 = 720;;
