(** 
Exercise 3.2 トップレべルでの以下の 2 種類の宣言の違いは何か?(ヒント: e2 が x を含む場合を考えよ.)
letx = e1 and y = e2;;
letx = e1 let y = e2;;
 *)
(* e1 = 1, e2 = x とすると、let x = 1 let y = x は y が 1 と定義されるが、
let x = 1 and let y = x は x と y を'同時に'定義するため、 y は x を参照できず、エラーとなる。*)
