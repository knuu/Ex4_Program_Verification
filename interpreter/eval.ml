open Syntax 

type exval = 
    IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t ref
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ProcV (i, e, t) -> "<fun>"

let pp_val v = print_string (string_of_exval v)

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Minus, IntV i1, IntV i2 -> IntV (i1 - i2)
  | Minus, _, _ -> err ("Both arguments must be integer: -")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Div, IntV i1, IntV i2 -> (try IntV (i1 / i2) with 
				Division_by_zero -> err ("Division_by_zero"))
  | Div, _, _ -> err ("Both arguments must be integer: /")
  | Mod, IntV i1, IntV i2 -> (try IntV (i1 mod i2) with 
				Division_by_zero -> err ("Division_by_zero"))
  | Mod, _, _ -> err ("Both arguments must be integer: mod")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, BoolV i1, BoolV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer or boolean: <")
  | Gt, IntV i1, IntV i2 -> BoolV (i1 > i2)
  | Gt, BoolV i1, BoolV i2 -> BoolV (i1 > i2)
  | Gt, _, _ -> err ("Both arguments must be integer or boolean: >")
  | Lte, IntV i1, IntV i2 -> BoolV (i1 <= i2)
  | Lte, BoolV i1, BoolV i2 -> BoolV (i1 <= i2)
  | Lte, _, _ -> err ("Both arguments must be integer or boolean: <=")
  | Gte, IntV i1, IntV i2 -> BoolV (i1 >= i2)
  | Gte, BoolV i1, BoolV i2 -> BoolV (i1 >= i2)
  | Gte, _, _ -> err ("Both arguments must be integer or boolean: >=")
  | Eq, IntV i1, IntV i2 -> BoolV (i1 = i2)
  | Eq, BoolV i1, BoolV i2 -> BoolV (i1 = i2)
  | Eq, _, _ -> err ("Both arguments must be integer or boolean: =")
  | Ne, IntV i1, IntV i2 -> BoolV (i1 <> i2)
  | Ne, BoolV i1, BoolV i2 -> BoolV (i1 <> i2)
  | Ne, _, _ -> err ("Both arguments must be integer or boolean: <>")
  | And, BoolV b1, BoolV b2 -> BoolV (b1 && b2)
  | And, _, _ -> err ("Both arguments must be boolean: &&")
  | Or, BoolV b1, BoolV b2 -> BoolV (b1 || b2)
  | Or, _, _ -> err ("Both arguments must be boolean: ||") 

let rec apply_prim_unary op arg = match op, arg with
    Not, BoolV b -> BoolV (not b)
  | Not, _ -> err("An arguments must be boolean: not")
  | UMinus, IntV i -> IntV (- i)
  | UMinus, _ -> err("An arguments must be integer: -")

let rec eval_exp env = function
    Var x -> 
      (try Environment.lookup x env with 
        Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | UnaryOp (op, exp) ->
     let arg = eval_exp env exp in
     apply_prim_unary op arg
  | BinOp (op, exp1, exp2) -> 
      let arg1 = eval_exp env exp1 in
      let arg2 = eval_exp env exp2 in
      apply_prim op arg1 arg2
  | IfExp (exp1, exp2, exp3) ->
      let test = eval_exp env exp1 in
        (match test with
            BoolV true -> eval_exp env exp2 
          | BoolV false -> eval_exp env exp3
          | _ -> err ("Test expression must be boolean: if"))
  | LetExp (id, exp1, exp2) ->
     let value = eval_exp env exp1 in
     eval_exp (Environment.extend id value env) exp2
  | FunExp (id, exp) -> 
     (match id with 
	ParaList (para, paralist) -> 
	ProcV(id, eval_exp env (FunExp (paralist, exp)), ref env)
       | _ -> ProcV (id, exp, ref env))
  | AppExp (exp1, exp2) ->
     let funval = eval_exp env exp1 in
     let arg = eval_exp env exp2 in
     (match funval with
      | ProcV (id, body, env') ->
	 let newenv = Environment.extend id arg !env' in
	 eval_exp newenv body
      | _ -> err ("Non-function value is applied"))
  | LetRecExp (id, para, exp1, exp2) ->
     let dummyenv = ref Environment.empty in
     let newenv = Environment.extend id (ProcV (para, exp1, dummyenv)) env in
     dummyenv := newenv;
     eval_exp newenv exp2

let rec eval_decl env expr = 
  try (match expr with
	  Exp e -> let v = eval_exp env e in ("-", env, v, "")
	| DeclList (decl, decllist) ->
	   let (id, newenv, v, _) = eval_decl env decl in
	   eval_decl newenv decllist
	| Decl (id, exp) ->
	   let v = eval_exp env exp in (id, Environment.extend id v env, v, "")
	| RecDecl (id, para, exp) ->
	   let dummyenv = ref Environment.empty in
	   let newenv = Environment.extend id (ProcV (para, exp, dummyenv)) env in
	   dummyenv := newenv; 
	   (id, newenv, (ProcV (para, exp, dummyenv)), "") ) with
    Error s -> ("exception", env, IntV 1, "Exception: " ^ s)
  | Failure s -> ("exception", env, IntV 1, "Exception: Failure " ^ s)
		 
;;
