open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t

type subst = (tyvar * ty) list

let rec subst_type ty_pair typ =
  match typ with
    TyVar v -> (match ty_pair with
		  [] -> typ
		| (tv, retty) :: res -> 
		   (if tv = v then subst_type res retty
		    else subst_type res typ))
  | TyFun (ty1, ty2) -> TyFun (subst_type ty_pair ty1, subst_type ty_pair ty2)
  | _ -> typ

(* eqs_of_subst : subst -> (ty * ty) list
型代入を型の等式集合に変換 *)
let eqs_of_subst s =
  List.map (fun x -> let (tyv, typ) = x in (TyVar tyv, typ)) s

(* subst_eqs: subst -> (ty * ty) list -> (ty * ty) list
型の等式集合に型代入を適用 *)
let rec subst_eqs s eqs = 
  List.map (fun x -> let (ty1, ty2) = x in 
		      ((subst_type s ty1), (subst_type s ty2))) eqs

(* 単一化アルゴリズム val unify : (ty * ty) list -> subst *)
let rec unify ty_list =
  match ty_list with
    [] -> []
  | (ty1, ty2) :: rest ->
     (match ty1, ty2 with
	TyInt, TyInt | TyBool, TyBool -> unify rest
      | TyVar v1, TyVar v2 -> 
	 if v1 = v2 then unify rest
	 else (v1, ty2) :: (unify (subst_eqs [(v1, ty2)] rest))
      | TyVar v, typ | typ, TyVar v -> 
          if MySet.member v (freevar_ty typ) then err("Type Error: cannot unify: in ftv")
	  else (v, typ) :: (unify (subst_eqs [(v, typ)] rest))
      | TyFun (t1, t2), TyFun (t1', t2') -> unify ([(t1, t1'); (t2, t2')] @ rest)
      | _ -> err ("Type Error: cannot unify"))
     
let ty_prim op ty1 ty2 = match op with
    Plus | Minus | Mult | Div | Mod 
        -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
    | Lt | Gt | Lte | Gte | Eq | Ne  
        -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
    | And | Or 
        -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)

let rec ty_exp tyenv = function 
    Var x ->
    (try ([], Environment.lookup x tyenv) with
       Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | BinOp (op, exp1, exp2) ->
     let (s1, ty1) = ty_exp tyenv exp1 in
     let (s2, ty2) = ty_exp tyenv exp2 in
     let (eqs3, typ) = ty_prim op ty1 ty2 in
     let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
     let s3 = unify eqs in (s3, subst_type s3 typ)
  | IfExp (exp1, exp2, exp3) ->
     let (s1, ty1) = ty_exp tyenv exp1 in
     let (s2, ty2) = ty_exp tyenv exp2 in
     let (s3, ty3) = ty_exp tyenv exp3 in
     let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) @ [(ty1, TyBool); (ty2, ty3)] in
     let s4 = unify eqs in (s4, subst_type s4 ty2)
  | LetExp (id, exp1, exp2) ->
     let (s1, ty1) = ty_exp tyenv exp1 in
     let newenv = Environment.extend id ty1 tyenv in
     let (s2, ty2) = ty_exp newenv exp2 in
     let domty = TyVar (fresh_tyvar ()) in
     let eqs = (eqs_of_subst s1) @ [ty1, domty] @ (eqs_of_subst s2) in
     let s3 = unify eqs in (s3, subst_type s3 ty2)
  | FunExp (id, exp) ->
     let domty = TyVar (fresh_tyvar ()) in
     let s, ranty =
       ty_exp (Environment.extend id domty tyenv) exp in
     (s, TyFun (subst_type s domty, ranty))
  | AppExp (exp1, exp2) -> 
     let (s1, ty1) = ty_exp tyenv exp1 in
     let (s2, ty2) = ty_exp tyenv exp2 in
     let domty = TyVar (fresh_tyvar ()) in
     let eqs = [(ty1, TyFun (ty2, domty))] @ (eqs_of_subst s1) @ (eqs_of_subst s2) in
     let s3 = unify eqs in (s3, subst_type s3 domty)
  | _ -> err ("Not Implemented!")     

let ty_decl tyenv = function
    Exp e -> ty_exp tyenv e
  | _ -> err ("Not Implemented!")
