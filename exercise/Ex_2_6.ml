(** 
Exercise 2.6 次の関数を定義せよ.実数の切り捨てを行う関数 floor を用いてよい.
1. USドル(実数)を受け取って円(整数)に換算する関数(ただし1円以下四捨五入).(入力は小数 点以下 2 桁で終わるときに働けばよい.) レートは 1$ = 111.12 円とする.
2. 円 (整数) を受け取って,US ドル (セント以下を小数にした実数) に換算する関数 (ただし 1 セ ント以下四捨五入).レートは 1$ = 111.12 円とする.
3. US ドル (実数) を受け取って,文字列 "⟨ ドル ⟩ dollars are ⟨ 円 ⟩ yen." を返す関数.
4. 文字を受け取って,アルファべットの小文字なら大文字に,その他の文字はそのまま返す関数
capitalize.(例: capitalize 'h' ⇒ 'H', capitalize '1' ⇒ '1')
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
