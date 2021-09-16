let t = Array.make 28124 0 ;;
let sum_fac n =
  let rec aux n i l =
    if (i = n ) then
      (t.(n) <- l;l)
    else if (i=1 && t.(n) <> 0) then t.(n) 
    else if (n mod i = 0) then (aux n (i+1) (i+l))
    else (aux n (i+1) l) in
  aux n 1 0 ;;
let abundant n = (sum_fac n) > n ;;
let ab = Array.make 28124 false ;;
for i=1 to 28123 do
  ab.(i) <- abundant i ;
done ;
ab ;;
let sum = Array.make 56246 false ;;
for i=1 to 28123 do
  if (ab.(i)) then
    for j=i to 28123 do
      if (ab.(j)) then
        sum.(i+j) <- true;
    done;
done;
let counter = ref 0 in
for i=1 to 28123 do
  if (not sum.(i)) then counter := !counter + i ;
done ;
!counter ;;
