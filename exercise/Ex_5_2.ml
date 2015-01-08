(**
Exercise 5.2 sum_list,max_list ��,match ��Ȥ鷺 null, hd, tl ���ȹ�碌�Τߤ��������. match ��Ȥ��ƥ����Ȥ���������,�����̤ʤɤ������������������.
 *)
let hd (x::rest) = x;;
let tl (x::rest) = rest;;
let null = function [] -> true | _ -> false;;

let rec sum_list l =
  if null l then 0 else hd l + sum_list (tl l)
;;

let rec max_list l =
  if null (tl l) then hd l
  else if hd l > hd (tl l) then max_list ((hd l) :: (tl (tl l))) else max_list (tl l)
;;

(* �쥸��������񤭴��������� *)
(*
sum_list [1;2;3;4;5;6;7;8;9;10] = 55;;
max_list [1;5;7;2;1;5;7;8;4;2;5;7] = 8;;
 *)
(* ñ�ʤ�ư���ǧ *)

(*
null,hd,tl �ʤɤ�Ȥäƽ񤯤ȡ�hd �� tl �����ߤ䤿���¿���ʤ뷹��������褦�˻פ��롣max_list�ˤ����Ƥ���ϸ����Ǥ��롣
matchʸ��Ȥä���������̤��ʬ�व��Ƥ��ꡢ���ˤ狼��䤹�����ޤ����ʷ�Ǥ��롣����������ϡ�List�����(����)���Ѥ�äƤ��ޤ��ȡ��ꥹ�ȤˤĤ���matchʸ�ǽ񤫤줿�Ȥ�������ƽ������ʤ���Фʤ�ʤ����Ȥ��̣�����ݼ���礭�ʥ����Ȥ������äƤ��ޤ����������ˤ����ơ�null,hd,tl ��Ȥäƽ񤤤����ϡ�null, hd, tl ������������Ǥ褯���ݼ�����ˤ�����ͥ��Ƥ���ȸ����롣
 *)
