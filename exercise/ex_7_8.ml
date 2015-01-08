(**
Exercise 7.8 �ʲ��δؿ� change ��,�����֤������״ؿ��Ǥ���.
# let rec change = function
      (_, 0) -> []
    | ((c :: rest) as coins, total) ->
        if c > total then change (rest, total)
        else c :: change (coins, total - c);;
Warning P: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
([], 1)
  .................function
      (_, 0) -> []
    | ((c :: rest) as coins, total) ->
        if c > total then change (rest, total)
    else c :: change (coins, total - c)..
val change : int list * int -> int list = <fun>
Ϳ����줿 (�߽�ˤʤ���) �̲ߤΥꥹ�� coins �ȹ�׶�� total ���饳����Υꥹ�Ȥ��֤�.
# let us_coins = [25; 10; 5; 1]
  and gb_coins = [50; 20; 10; 5; 2; 1];; val us_coins : int list = [25; 10; 5; 1]
val gb_coins : int list = [50; 20; 10; 5; 2; 1]
# change (gb_coins, 43);;
- : int list = [20; 20; 2; 1]
# change (us_coins, 43);;
- : int list = [25; 10; 5; 1; 1; 1]
������,�����������Ƭ�ˤ��륳�����Ǥ���¤�Ȥ����Ȥ��뤿��,��ǽ�ʥ�������ȹ�碌������Ȥ��ˤǤ⼺�Ԥ��Ƥ��ޤ����Ȥ�����.
# change ([5; 2], 16);;
Exception: Match_failure ("", 201, 17).
�����,�㳰�������Ѥ��Ʋ򤬤�����ˤϽ��Ϥ���褦�ˤ�����.�ʲ��Υץ�����,2�Ľ��...��ʬ�����,�ץ�����������Ԥ�.
let rec change = function
    (_, 0) -> []
  | ((c :: rest) as coins, total) ->
      if c > total then change (rest, total)
      else
        (try
          c :: change (coins, total - c)
     with Failure "change" -> ...)
  | _ -> ...;;
 *)

let rec change = function
  | (_, 0) -> []
  | ((c :: rest) as coins, total) ->
     if c > total then change (rest, total)
     else
       (try
           c :: change (coins, total - c)
	 with Failure "change" -> change (rest, total))
  | _ -> failwith "change"
;;

(* ������ä������㳰���ꤲ�ơ����Υ������Ȥ� *)

let us_coins = [25; 10; 5; 1]
and gb_coins = [50; 20; 10; 5; 2; 1];;
(*
change (gb_coins, 43) = [20; 20; 2; 1];;
change (us_coins, 43) = [25; 10; 5; 1; 1; 1];;
change ([5; 2], 16) = [5; 5; 2; 2; 2];;
 *)
(* gd_coins��us_coins�Ǥ�change��ư���ǧ�ȡ�����ˡ�Ǽ��Ԥ�����Υƥ��� *)

