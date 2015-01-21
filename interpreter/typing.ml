open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t

let print_op = function 
    Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "mod"
  | Lt -> "<"
  | Gt -> ">"
  | Lte -> "<="
  | Gte -> ">="
  | Eq -> "="
  | Ne -> "<>"
  | And -> "&&"
  | Or -> "||"

type subst = (tyvar * ty) list

let rec subst_type subst ty =
  match ty with
    TyVar v -> (match subst with
		  [] -> err ("variable not bound")
		| (tv, retty) :: res -> (if tv = v then subst_type res retty
					else subst_type res ty))
  | TyFun (ty1, ty2) -> TyFun (subst_type subst ty1, subst_type subst ty2)
  | _ -> ty
(*
(* eqs_of_subst : subst -> (ty * ty) list
￼型代入を型の等式集合に変換 *)
let eqs_of_subst s = 
(* subst_eqs: subst -> (ty * ty) list -> (ty * ty) list
型の等式集合に型代入を適用 *)
let subst_eqs s eqs = 
 *)
let rec unify ty_list =
  match ty_list with
    [] -> []
  | (ty1, ty2) :: rest ->
     (match ty1, ty2 with
	TyInt, TyInt | TyBool, TyBool -> unify rest
      | TyInt, TyVar v -> (unify rest) @ [(v, TyInt)]
      | TyBool, TyVar v -> (unify rest) @ [(v, TyBool)]
      | TyVar v, TyInt -> (unify rest) @ [(v, TyInt)]
      | TyVar v, TyBool -> (unify rest) @ [(v, TyBool)]
      | TyFun (t1, t2), TyFun (t1', t2') -> unify ([(t1, t1'); (t2, t2')] @ rest)
      | _ -> err ("Type Error: cannot unify"))
     
let ty_prim op ty1 ty2 = match op with
    Plus | Minus | Mult | Div | Mod 
        -> (match ty1, ty2 with
	      TyInt, TyInt -> TyInt
	    | _ -> err ("Argument must be of integer: " ^ (print_op op)))
    | Lt | Gt | Lte | Gte | Eq | Ne
        -> (match ty1, ty2 with
	      TyInt, TyInt -> TyBool
	    | _ -> err ("Argument must be of integer: " ^ (print_op op)))
    | And | Or
	-> (match ty1, ty2 with
	      TyBool, TyBool -> TyBool
	    | _ -> err("Argument must be of boolean: " ^ (print_op op)))

let rec ty_exp tyenv = function 
    Var x ->
    (try Environment.lookup x tyenv with
       Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, exp1, exp2) ->
     let tyarg1 = ty_exp tyenv exp1 in
     let tyarg2 = ty_exp tyenv exp2 in
     ty_prim op tyarg1 tyarg2
  | IfExp (exp1, exp2, exp3) ->
     let tyexp1 = ty_exp tyenv exp1 in
     let tyexp2 = ty_exp tyenv exp2 in
     let tyexp3 = ty_exp tyenv exp3 in
     (match tyexp1 with
	TyBool -> (match tyexp2, tyexp3 with
		     TyInt, TyInt -> TyInt
		   | TyBool, TyBool -> TyBool
		   | _, _ -> err ("'If' expression must return the same type"))
      | _ -> err ("'If' condition must be of boolean"))	
  | LetExp (id, exp1, exp2) ->
     let tyid = ty_exp tyenv exp1 in
     let newenv = Environment.extend id tyid tyenv in
     ty_exp newenv exp2
  | _ -> err ("Not Implemented!")     

let ty_decl tyenv = function
    Exp e -> ty_exp tyenv e
  | _ -> err ("Not Implemented!")
