let t = Array.make 10001 0 ;;
let sum_fac n =
  let rec aux n i l =
    if (i = n ) then
      (t.(n) <- l;l)
    else if (i=1 && t.(n) <> 0) then t.(n) 
    else if (n mod i = 0) then (aux n (i+1) (i+l))
    else (aux n (i+1) l) in
  aux n 1 0 ;;
let res = ref 0 in
for i=1 to 10000 do
  for j=i+1 to 10000 do
    let a = (sum_fac i) and b = (sum_fac j) in
    if (a=j && b=i) then
      res := !res + a + b ;
  done;
done;
Printf.printf "%d\n" !res ;;
