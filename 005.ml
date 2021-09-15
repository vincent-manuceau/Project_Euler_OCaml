let p = [| 2; 3 ; 5 ; 7 ;11 ;13 ;17 ; 19 |] ;;
let max = [|0; 0; 0; 0; 0 ;0 ;0 ;0|] ;;

let power x n = 
 let res = ref 1 in
 for i=1 to n do
  res := !res * x ;
 done;
 !res ;;

for i=2 to 20 do
 for j=0 to Array.length p - 1 do
  let cur = p.(j) in
  if (i >= cur) then
  (
   let quot = ref (i/cur) and rem = ref (i mod cur) 
   and pow = ref 0 in
   if (!quot > 0 && !rem = 0) then
    incr pow ; 
   while(!quot > 0 && !rem = 0) do
    rem := !quot mod cur ;
    quot := !quot / cur ;
    
    if (!quot > 0 && !rem = 0) then
     (incr pow ;)
   done ;
   if (!pow > max.(j)) then
    max.(j) <- !pow ;
  )
 done ;
done ;;

let res = ref 1 in
for i=0 to Array.length p -1 do
 res := !res * (power p.(i) max.(i));
done;
Printf.printf "%d\n" !res ;;
