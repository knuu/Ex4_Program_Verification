open Syntax
open Eval
open Typing

let rec read_eval_print env tyenv =
  print_string "# ";
  flush stdout;
  try 
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let ty = ty_decl tyenv decl in
    let (id, newenv, v, errmsg) = eval_decl env decl in
    if id = "exception" then (
      Printf.printf "%s" errmsg;
      print_newline();
      read_eval_print newenv tyenv)
    else ( 
      Printf.printf "val %s : " id; 
      print_string (pp_ty ty);
      print_string " = ";
      pp_val v;
      print_newline();
      read_eval_print newenv tyenv)
  with
    Parsing.Parse_error -> 
    Printf.printf "Error: Syntax error";
    print_newline();
    read_eval_print env tyenv
   | Failure s ->
     Printf.printf "Error: %s" s;
     print_newline();
     read_eval_print env tyenv
  | Typing.Error s ->
     Printf.printf "Error: %s" s;
     print_newline();
     read_eval_print env tyenv
     

let initial_env = 
  Environment.extend "i" (IntV 1)
 (Environment.extend "v" (IntV 5) 
 (Environment.extend "x" (IntV 10) 
 (Environment.extend "ii" (IntV 2)
 (Environment.extend "iii" (IntV 3)
 (Environment.extend "iv" (IntV 4)
Environment.empty)))))

let initial_tyenv = 
  Environment.extend "i" TyInt
 (Environment.extend "v" TyInt
 (Environment.extend "x" TyInt
 (Environment.extend "ii" TyInt
 (Environment.extend "iii" TyInt
 (Environment.extend "iv" TyInt
Environment.empty)))))

let _ = read_eval_print initial_env initial_tyenv
