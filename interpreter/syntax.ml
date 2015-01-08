(* ML interpreter / type reconstruction *)
type id = string

type unaryOp = UMinus | Not
type binOp = Plus | Minus | Mult | Div | Mod | Lt | Gt | Lte | Gte | Eq | Ne | And | Or 

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | UnaryOp of unaryOp * exp
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp
  | FunExp of id * exp
  | AppExp of exp * exp
  | LetRecExp of id * id * exp * exp

type program = 
    Exp of exp
  | Decl of id * exp
  | RecDecl of id * id * exp


