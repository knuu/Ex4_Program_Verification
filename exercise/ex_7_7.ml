(**
Exercise 7.7 ��Ǥߤ����֥������Ȼظ����ץ���ߥ󥰤α�Ĺ�ǷѾ��ʤɤ�ɽ�����Ƥߤ褦�Ȼפ�.�ʲ��Ͽ���ɽ�����ȿ��դ������֥������ȤΥ��󥿡��ե������Ǥ���.
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
���դ������֥������ȤϺ�ɸ�˲ä�,������֤Ȥ��Ƥ�ĤȤ���.�ޤ�,cget, cinc �� pointC �� �᥽�åɤ�Ѿ���,cset �Ϻ�ɸ�Υ��åȤȤȤ�˿�����˥��åȤ���褦�˼���������.�ʲ������λ�ߤǤ���.
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
������,���μ����Ϥ��ޤ�Ư���ʤ�.
# let cp = new_cpoint 0 Red;;
val cp : cpointI =
  {cget = <fun>; cset = <fun>; cinc = <fun>; getcolor = <fun>}
# cp.cinc();;
- : unit = ()
# cp.cget();;
- : int = 1
# cp.getcolor();;
- : color = Red
cinc ��Ǥ�,��ɸ�򥻥åȤ���Τǿ�����ˤʤäƤ��Ƥۤ����Τ˸��ΤޤޤǤ���.������ͳ��� �����ε�ư�ȤȤ��������,���ޤ� cinc ����ư����褦�˥ץ�����񤭴�����.������, �Ѿ������路�����Τ�,Ʊ�����Ȥ򤹤�᥽�åɤ���������ؿ��ϰ��ٽ񤯤����� (��� super.get �Τ褦�ʷ���) �����Ѥ��뤳��.
(�ҥ��: pointC �������ʲ��Τ褦���ѹ�����.)
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
cinc��super.inc���Ĥޤ�pointC��inc��ƤӽФ��Ƥ��롣�����ǸƤӽФ����Τ�cpointC��cinc�ǤϤʤ���pointC��inc�Ǥ��뤿�ᡢ�����Ѥ��ʤ���
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
