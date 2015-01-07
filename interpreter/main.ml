open Syntax
open Eval

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  try 
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let (id, newenv, v, errmsg) = eval_decl env decl in
    if id = "exception" then (
      Printf.printf "%s" errmsg;
      print_newline();
      read_eval_print newenv )
    else ( 
      Printf.printf "val %s = " id; 
      pp_val v;
      print_newline();
      read_eval_print newenv)
  with
    Parsing.Parse_error -> 
    Printf.printf "Error: Syntax error";
    print_newline();
    read_eval_print env
  | Failure s ->
     Printf.printf "Error: Syntax error";
     print_newline();
     read_eval_print env

let initial_env = 
  Environment.extend "i" (IntV 1)
 (Environment.extend "v" (IntV 5) 
 (Environment.extend "x" (IntV 10) 
 (Environment.extend "ii" (IntV 2)
 (Environment.extend "iii" (IntV 3)
 (Environment.extend "iv" (IntV 4)
Environment.empty)))))

let _ = read_eval_print initial_env
