(** Ex 5.1 *)
(* 1: OK *)
(* 2: NG
リストの各要素の型は一致しなければいけないが、[1;3] がint型のリストで、["hoge"]はstring型のリストであるから。(リストの要素である、各リストの要素の型は全てのリストで一致しないといけない)
 *)
(* 3: OK *)
(* 4: NG
a::b::cとした場合、aとbは同じhoge型で、cはhoge型のリストでなければいけない。
 *)
(* 5: OK (1と同じ)*)
(* 6: OK (二番目の要素でbool->boolの関数のリストになる)*)
