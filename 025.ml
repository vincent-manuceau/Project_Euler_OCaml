let target = 1000 in
let f = Array.make_matrix 3 (target+1) 0 and n = ref 3
and run = ref true in

f.(1).(0) <- 1 ;
f.(2).(0) <- 1 ;

while (!run) do
  let rem = ref 0  and i = ref 0 in
  while (!i <= target) do
    let sum = !rem + (f.((!n-2) mod 3).(!i) +
                      (f.((!n-1) mod 3).(!i))) in
    rem := sum / 10 ;
    f.(!n mod 3).(!i) <- sum mod 10 ;
    incr i ;
  done ;
  
  if (f.(!n mod 3).(target-1) <> 0) then
    run := false
  else
    incr n ;
done ;
Printf.printf "%d\n"(!n) ;;

