(**
Exercise 7.6 7.1.4 節で述べた [] への参照の例を実際にインタラクティブ・コンパイラで試し,テキストに書いた挙動との違い,特に,参照の型を説明し,どのようにして true を [1] に cons してしまうような事態の発生が防がれているか説明せよ.
 *)

(*
let x = ref [];;
で定義されたときの型は、
val x : 'a list ref = {contents=[]}
ではなく
val x : '_a list ref = {contents=[]}
である。つまり、xがある型のリストであると決まれば、それ以降で型は変更できない。
よって、
(2 :: !x, true :: !x)
は、まず第一要素でxはint型のリストであると推測されるため、第二要素でboolとconsしようとするとエラーになる。
また、
x := [1]
とすると、そこでxはint型のリストと決まるため、
true :: !x
はbool型の要素をint型のリストとconsさせようとしているため、エラーとなる。
 *)
