let max = 105000 in
let c = Array.make max 1 in
for i=2 to max-1 do
 for j=2 to max/i - 1 do
  c.(i*j) <- 0 ;
 done;
done;

let s = ref (0) and num_p = ref 0 and i= ref 2 in

while ((!i < max) && (!s < 10001)) do
 if (c.(!i) = 1) then 
 begin
  num_p := !i ;
  incr s ;
 end ;
 incr i ;
done ;

(!num_p,!s) ;;
