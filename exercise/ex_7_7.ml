(**
Exercise 7.7 上でみたオブジェクト指向風プログラミングの延長で継承などを表現してみようと思う.以下は色を表す型と色付き点オブジェクトのインターフェースである.
# type color = Blue | Red | Green | White;;
type color = Blue | Red | Green | White
# type cpointI = {cget: unit -> int;
                  cset: int -> unit;
                  cinc: unit->unit;
                  getcolor: unit-> color};;
type cpointI = {
  cget : unit -> int;
  cset : int -> unit;
  cinc : unit -> unit;
  getcolor : unit -> color;
}
色付き点オブジェクトは座標に加え,色を状態としてもつとする.また,cget, cinc は pointC の メソッドを継承し,cset は座標のセットとともに色を白にセットするように実装したい.以下がその試みである.
# let cpointC x col=
    let super = pointC x in
    let rec this =
   {cget=  super.get;
       cset=  (fun x -> super.set x; col := White);
       cinc= super.inc;
       getcolor = (fun () -> !col)} in
  this;;
val cpointC : int ref -> color ref -> cpointI = <fun>
# let new_cpoint x col = cpointC (ref x) (ref col);;
val new_cpoint : int -> color -> cpointI = <fun>
しかし,この実装はうまく働かない.
# let cp = new_cpoint 0 Red;;
val cp : cpointI =
  {cget = <fun>; cset = <fun>; cinc = <fun>; getcolor = <fun>}
# cp.cinc();;
- : unit = ()
# cp.cget();;
- : int = 1
# cp.getcolor();;
- : color = Red
cinc 中では,座標をセットするので色は白になっていてほしいのに元のままである.この理由をフ ロクラムの挙動とともに説明し,うまく cinc が作動するようにプログラムを書き換えよ.ただし, 継承を模倣したいのて,同じことをするメソッドに相当する関数は一度書くだけで (上の super.get のような形て) 再利用すること.
(ヒント: pointC の定義を以下のように変更する.)
# let pointC x this () =
    {get=  (fun () -> !x);
     set=  (fun newx -> x:=newx);
     inc= (fun () -> (this ()).set ((this ()).get () + 1))};;
val pointC : int ref -> (unit -> pointI) -> unit -> pointI = <fun>
# let new_point x =
    let x = ref x in
    let rec this () = pointC x this () in
  this ();;
val new_point : int -> pointI = <fun>
 *)

(* 
cincでsuper.inc、つまりpointCのincを呼び出している。そこで呼び出されるのはcpointCのcincではなく、pointCのincであるため、色は変わらない。
 *)
(*
type pointI = {get: unit -> int; set: int -> unit; inc: unit->unit};;
(*
let pointC x =
  let rec this () =
    {get=  (fun () -> !x);
     set=  (fun newx -> x:=newx);
     inc= (fun () -> (this ()).set ((this ()).get () + 1))} in
  this ();;

let new_point x = pointC (ref x);;
 *)

let pointC x this () =
  {get=  (fun () -> !x);
   set=  (fun newx -> x:=newx);
   inc= (fun () -> (this ()).set ((this ()).get () + 1))};;
let new_point x =
  let x = ref x in
  let rec this () = pointC x this () in
  this ();;
type color = Blue | Red | Green | White;;

type cpointI = {cget: unit -> int;
                cset: int -> unit;
                cinc: unit->unit;
                getcolor: unit-> color};;

let cpointC x col this () =
  let super = pointC x this () in
  {cget= super.get;
   cset= (fun x -> super.set x; col := White);
   cinc= super.inc;
   getcolor= (fun () -> !col)}
;;

let new_cpoint x col = 
  let rec this () = cpointC (ref x) (ref col) this () in
  this ()
;;
 *)
