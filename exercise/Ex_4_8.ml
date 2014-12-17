(**
Exercise 4.8 double double f x が f (f (f (f x))) として働く理由を前問と同様にして説明せよ
 *)

let double f x = f (f x);;
(*
double double f x
=> ((double double) f) x
=> (double (double f)) x 
=> (double f) ((double f) x)
=> (double f) (f (f x))
=> f (f (f (f x)))
 *)








