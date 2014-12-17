(**
Exercise 5.1 次のうち,正しいリスト表現はどれか.コンパイラに入力する前に,正しい場合と思う場合は式の型を,間違っていると思う場合はなぜ誤りか,を予想してから実際に確認せよ.
1. [[]]
2. [[1; 3]; ["hoge"]]
3. [3] :: []
4. 2 :: [3] :: []
5. [] :: []
6. [(fun x -> x); (fun b -> not b)]
 *)
(* 1: OK 型: 'a list list *)
(* 2: NG
リストの各要素の型は一致しなければいけないが、[1;3] がint型のリストで、["hoge"]はstring型のリストであるから。(リストの要素である、各リストの要素の型は全てのリストで一致しないといけない)
 *)
(* 3: OK 型: int list list *)
(* 4: NG
a::b::cとした場合、aとbは同じhoge型で、cはhoge型のリストでなければいけない。
 *)
(* 5: OK 型: 'a list list *)
(* 6: OK 型: (bool -> bool) list *)
