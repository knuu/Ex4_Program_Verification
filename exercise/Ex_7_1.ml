(**
Exercise 7.1 ref �����ͤ�ɽ���򸫤Ƶ��Ť��Ƥ���ͤ⤤�뤫�⤷��ʤ���,ref ���ϰʲ��Τ褦��������줿 1�ե�����ɤι�����ǽ�ʥ쥳���ɤǤ���. 
type 'a ref = { mutable contents : 'a };;
�ؿ� ref, ���֥��ڥ졼��!,���֥��ڥ졼�� := �������,�쥳���ɤ˴�Ϣ�������ǽ�.
 *)

let ref x = { contents = x }
let (!) x = x.contents;;
let (:=) x y = x.contents <- y;;

(* �񤯤��� *)

(* 
let a = ref 3;;
a := 6;;
!a = 6;;
 *)

(* ñ�ʤ�ư���ǧ *) 
