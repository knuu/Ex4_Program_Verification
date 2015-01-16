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

type ty =
    TyInt
  | TyBool

let pp_ty = function
    TyInt -> print_string "int"
  | TyBool -> print_string "bool"



