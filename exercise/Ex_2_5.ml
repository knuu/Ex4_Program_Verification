(** Ex 2.5 *)
let a_2' = 1;; (* OK *)
let ____ = 1;; (* OK *)
let Cat = 1;; (* 1文字目は大文字はダメ *)
let _'_'_ = 1;; (* OK *)
let 7eleven = 1;; (* 1文字目は数字はダメ *)
let 'ab2_ = 1;; (* 1文字目はプライムはダメ *)
let _ = 1;; (* アンダースコアのみはダメ *)


