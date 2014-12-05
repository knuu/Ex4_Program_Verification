(** Ex 2.6 *)
(* 1 *)
let dollar_to_yen c = (* �ɥ뤫��ߤ� *)
  int_of_float (c *. 111.12 +. 0.5)
;;

(* 2 *)
let yen_to_dollar c = (* �ߤ���ɥ�� *)
  floor((float_of_int c /. 111.12) *. 100.0 +. 0.5) /. 100.0
;;

(* 3 *)
let exchange_d_to_y c =
  string_of_float c ^ " dollars are " ^ string_of_int (dollar_to_yen c) ^ " yen."
;;

(* 4 *)
let capitalize c = (* ��ʸ������ʸ�����Ѵ�����ؿ� *)
  if
    'a' <= c && c <= 'z'
  then
    let to_cap = int_of_char 'A' - int_of_char 'a' in
    char_of_int(int_of_char c + to_cap)
  else
    c
;;
