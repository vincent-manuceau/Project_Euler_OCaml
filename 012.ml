let factor x =
 let n = ref 2 and l = ref [1] in
 while( !n <= x/2) do
  if (x mod !n = 0) then
   l := !n :: !l ;
  incr n 
 done ;
 x::!l
;;

let tri n = n * (n+1) / 2 ;;

let a n = List.length (factor (tri n)) ;;

let i = ref 0 and res = ref 0 in
let max = ref 0 in 
while !res < 500 do
 incr i ;
 res := a !i;   
 if (!res > !max) then
  max := !res ;
done ;
Printf.printf "\n%d : %d (%d)\n" !i (tri !i) !res;;
