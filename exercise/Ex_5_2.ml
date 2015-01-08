(**
Exercise 5.2 sum_list,max_list を,match を使わず null, hd, tl の組合わせのみで定義せよ. match を使うテキストの定義と比べ,記述面などの利害得失を議論せよ.
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

(* レジメの定義を書き換えただけ *)
(*
sum_list [1;2;3;4;5;6;7;8;9;10] = 55;;
max_list [1;5;7;2;1;5;7;8;4;2;5;7] = 8;;
 *)
(* 単なる動作確認 *)

(*
null,hd,tl などを使って書くと、hd や tl がむやみやたらに多くなる傾向があるように思える。max_listにおいてこれは顕著である。
match文を使った式は定義通りに分類されており、非常にわかりやすく、また、簡潔である。しかしそれは、Listの定義(実装)が変わってしまうと、リストについてmatch文で書かれたところを全て修正しなければならないことを意味し、保守に大きなコストがかかってしまう。その点において、null,hd,tl を使って書いた場合は、null, hd, tl を修正するだけでよく、保守の点において優れていると言える。
 *)
