(**
Exercise 7.1 ref 型の値の表示を見て気づいている人もいるかもしれないが,ref 型は以下のように定義された 1フィールドの更新可能なレコードである. 
type 'a ref = { mutable contents : 'a };;
関数 ref, 前置オペレータ!,中置オペレータ := の定義を,レコードに関連した操作で書け.
 *)

let ref x = { contents = x }
let (!) x = x.contents;;
let (:=) x y = x.contents <- y;;

(* 書くだけ *)

(* 
let a = ref 3;;
a := 6;;
!a = 6;;
 *)

(* 単なる動作確認 *) 
