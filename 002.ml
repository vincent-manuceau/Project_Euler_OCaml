let rec fib u_n1 u_n s =
 let u_n2 = u_n1 + u_n in
 if (u_n2 >= 4000000) then
  s
 else if (u_n2 mod 2 = 0) then
  (fib u_n2 u_n1 (s + u_n2))
 else
  (fib u_n2 u_n1 s)
;;

Printf.printf "%d\n" (fib 2 1 2) ;;
