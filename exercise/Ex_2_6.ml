(** 
Exercise 2.6 次の関数を定義せよ.実数の切り捨てを行う関数 floor を用いてよい.
1. USドル(実数)を受け取って円(整数)に換算する関数(ただし1円以下四捨五入).(入力は小数点以下 2 桁で終わるときに働けばよい.) レートは 1$ = 111.12 円とする.
2. 円 (整数) を受け取って,US ドル (セント以下を小数にした実数) に換算する関数 (ただし 1 セント以下四捨五入).レートは 1$ = 111.12 円とする.
3. US ドル (実数) を受け取って,文字列 "<ドル> dollars are <円> yen." を返す関数.
4. 文字を受け取って,アルファベットの小文字なら大文字に,その他の文字はそのまま返す関数
capitalize.(例: capitalize 'h' => 'H', capitalize '1' => '1')
 *)

(* 1 *)
let dollar_to_yen dol = 
  int_of_float (dol *. 111.12 +. 0.5)
;;

(* 2 *)
let yen_to_dollar yen = 
  floor((float_of_int yen /. 111.12) *. 100.0 +. 0.5) /. 100.0
;;

(* 3 *)
let exchange_d_to_y dol =
  string_of_float dol ^ " dollars are " ^ string_of_int (dollar_to_yen dol) ^ " yen."
;;

(* 4 *)
let capitalize c = 
  if 'a' <= c && c <= 'z'
  then
    let to_cap = int_of_char 'A' - int_of_char 'a' in
    char_of_int(int_of_char c + to_cap)
  else c
;;

(*
四捨五入は、0.5を足して切り捨てすることで実装した。
2では小数点3桁目を四捨五入するために、一度100倍して四捨五入に100でわっている。
4のは小文字を大文字に変換するために、大文字と小文字の文字コードの差分(to_cap)を計算しておいて、小文字にto_capを足している。
 *)

(*
dollar_to_yen 112.0 = 12445;; 
dollar_to_yen 113.0 = 12557;;
yen_to_dollar 49390 = 444.47;;
yen_to_dollar 12346 = 111.11;;
exchange_d_to_y 112.0 =  "112. dollars are 12445 yen.";;
capitalize 'c' = 'C';;
capitalize 'C' = 'C';;
 *)

(*
上から4つは、それぞれ返り値がxxx.4 と xxx.5となるような数字を用いたテスト
5つ目は文字列を比較して動作するかの確認
6,7つ目は小文字と小文字でない文字についてテスト
 *)
