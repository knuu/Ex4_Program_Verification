(**
Exercise 6.7 �ʲ���,­�����ȳݤ�������ʤ�����ι�ʸ��ɽ����������Ǥ���.
# type arith =
    Const of int | Add of arith * arith | Mul of arith * arith;;
type arith = Const of int | Add of arith * arith | Mul of arith * arith
# (* exp stands for (3+4) * (2+5) *)
 let exp = Mul (Add (Const 3, Const 4), Add (Const 2, Const 5));;
val exp : arith = Mul (Add (Const 3, Const 4), Add (Const 2, Const 5))
������ʸ����ɽ�������ؿ� string_of_arith, ʬ��§���Ѥ��ƿ����� (i_11 * ... * i_1n1 ) + ... + (i_m1 * ... * i_mnm ) �η����ѷ�����ؿ� expand ���������.
# string_of_arith exp;;
- : string = "((3+4)*(2+5))"
# string_of_arith (expand exp);;
- : string = "(((3*2)+(3*5))+((4*2)+(4*5)))"
(���ץ����Ȥ���) string_of_arith �ν��Ϸ�̤γ�̤򸺤餹�褦�˹��פ���.(��ν�����Ǥ� ���⹩�פ��Ƥ��ʤ�.)
 *)

type arith = Const of int | Add of arith * arith | Mul of arith * arith;;
let exp = Mul (Add (Const 3, Const 4), Add (Const 2, Const 5));;

let rec string_of_arith = function
  | Const c -> string_of_int(c)
  | Add (a1, a2) -> "(" ^ string_of_arith(a1) ^ "+" ^ string_of_arith(a2) ^ ")"
  | Mul (m1, m2) -> "(" ^ string_of_arith(m1) ^ "*" ^ string_of_arith(m2) ^ ")"
;;

let rec expand = function
  | Const c -> Const c
  | Add (a1, a2) -> Add(expand(a1), expand(a2))
  | Mul (m1, m2) -> match (m1, m2) with
		    | (Const _, Const _) -> Mul(m1, m2)
		    | (Const _, Add(m1', m2')) -> 
		        Add(expand(Mul(m1, expand(m1'))), expand(Mul(m1, expand(m2'))))
		    | (Const _, Mul(_, _)) -> Mul(m1, expand(m2))
		    | (Add(a1', a2'), _) ->
		        Add(expand(Mul(a1', m2)), expand(Mul(a2', m2)))
		    | (Mul (_, _), _) -> Mul(expand(m1), expand(m2))
;;  
(* Test *)
let exp2 = Mul(Const 4, exp);;
string_of_arith exp;;
string_of_arith (expand exp);;
string_of_arith (expand exp2);;


