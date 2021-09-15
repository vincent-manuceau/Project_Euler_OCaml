let max = 2000000 in
let c = Array.make max 1 in
for i=2 to max-1 do
 for j=2 to (max-1)/i do
  if (i*j < max) then
   c.(i*j) <- 0 ;
 done;
done;
let sum = ref 0 in
for i=2 to max-1 do
 if c.(i) = 1 then begin
  sum := !sum + i ;
  Printf.printf "%d " i;
 end
done;
!sum ;;
