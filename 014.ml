let a = Array.make 1000001 0 ;;
a.(0) <- 1 ;
a.(1) <- 1 ;

let rec collatz n acc = 
 if (n = 1) then acc+1
 else if ((n < Array.length a) && (a.(n) <> 0)) then
  acc+a.(n)
 else if (n mod 2 = 0) then (collatz (n/2) (acc+1))
 else (collatz (3*n+1) (acc+1)) in

let max = ref 0 and max_n = ref 0 in
for i=2 to 1000000 do
 a.(i) <- collatz i 0 ;
 if (a.(i) > !max) then begin
  max := a.(i);
  max_n := i ;
 end;
done ;
(!max_n, !max) ;;
