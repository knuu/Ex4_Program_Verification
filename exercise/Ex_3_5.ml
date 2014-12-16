(** 
Exercise 3.5 次のふたつの型
float * float * float * float
(float * float) * (float * float)
の違いを,その型に属する値の構成法と,要素の取出し方からみて比較せよ.
 *)

(* 
1: float * float * float * float
2: (float * float) * (float * float)
とする。

・構成法
1は4つのfloat型の数を要素としてもつ組だが、2は2つの組をもつ組で、それぞれの組は2つのfloat型の数を要素としてもつような組である。
つまり、1は (1.0 ,2.0 ,3.0 ,4.0) というような組で、2は ((1.0, 2.0), (3.0, 4.0)) というような組である。

・要素の取り出し方
1は
let (x, y, z, w) = pair
2は
let ((x, y), (z, w)) = pair
というように取り出す。
 *)
