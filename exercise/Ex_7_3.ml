(** 
Exercise 7.3 以下で定義する funny_fact は再帰的定義 (rec) を使わずに階乗を計算している.どのような仕組みで実現されているか説明せよ.
# let f = ref (fun y -> y + 1)
  let funny_fact x = if x = 1 then 1 else x * (!f (x - 1));;
val f : (int -> int) ref = {contents = <fun>} val funny_fact : int -> int = <fun>
# f := funny_fact;;
- : unit = ()
# funny_fact 5;;
- : int = 120
 *)

(*
funny_fact関数で、階乗を再帰的に計算をする部分で呼び出す関数fを副作用が起こりうる変数として定義しておいて、その後、fにfunny_factを代入している。
つまり、単にrecを使わずに定義しているというだけで、funny_factが呼び出される回数などは(末尾さいきでない)再帰的定義のfactと変わらない。
 *)
