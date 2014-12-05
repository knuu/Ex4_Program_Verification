(** Ex 3.10 *)
let rec fib n = (* nth Fibonacci number *)
  if n = 1 || n = 2 then 1 else fib(n - 1) + fib(n - 2);;
(*
fib 4
-> if 4 = 1 || 4 = 2 then 1 else fib(4 - 1) + fib(4 - 2)
-> fib(4 - 1) + fib(4 - 2)
-> fib 3 + fib(4 - 2)
-> (if 3 = 1 || 3 = 2 then 1 else fib(3 - 1) + fib(3 - 2)) + fib(4 - 2) 
-> (fib(3 - 1) + fib(3 - 2)) + fib(4 - 2)
-> (fib 2 + fib(3 - 2)) + fib(4 - 2)
-> ((if 2 = 1 || 2 = 2 then 1 else fib(2 - 1) + fib(2 - 2)) + fib(3 - 2)) + fib(4 - 2)
-> (1 + fib(3 - 2)) + fib(4 - 2)
-> (1 + fib 1) + fib(4 - 2)
-> (1 + (if 1 = 1 || 1 = 2 then 1 else fib(1 - 1) + fib(1 - 2))) + fib(4 - 2)
-> (1 + 1) + fib(4 - 2)
-> 2 + fib(4 - 2)
-> 2 + fib 2
-> 2 + (if 2 = 1 || 2 = 2 then 1 else fib(2 - 1) + fib(2 - 2))
-> 2 + 1
-> 3
(* if の条件式の評価等、ある程度省略しているところはあります *)
 *)
