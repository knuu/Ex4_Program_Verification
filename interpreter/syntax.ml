(* ML interpreter / type reconstruction *)
type id = string

type unaryOp = UMinus | Not
type binOp = Plus | Minus | Mult | Div | Mod 
	     | Lt | Gt | Lte | Gte | Eq | Ne 
	     | And | Or 

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | UnaryOp of unaryOp * exp
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp
  | FunExp of id * exp
  | DFunExp of id * exp
  | AppExp of exp * exp
  | LetRecExp of id * id * exp * exp

type program = 
    Exp of exp
  | DeclList of program * program
  | Decl of id * exp
  | AndDecl of id * exp * program
  | RecDecl of id * id * exp

type tyvar = int

type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty

(* pretty printing *)
let rec pp_ty = function
    TyInt -> "int"
  | TyBool -> "bool"
  | TyVar v -> "'" ^ Char.escaped ((char_of_int ((int_of_char 'a') + v)))
  | TyFun (ty1, ty2) -> (pp_ty ty1) ^ " -> " ^ (pp_ty ty2)

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1; v
  in body

let rec freevar_ty ty = (* ty -> tyvar MySet.t *)
  match ty with
  | TyVar v -> MySet.singleton v
  | TyFun (ty1, ty2) -> MySet.union (freevar_ty ty1) (freevar_ty ty2)
  | _ -> MySet.empty



