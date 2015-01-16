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
