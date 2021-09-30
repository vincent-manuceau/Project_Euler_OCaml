let tSize = 3000 ;;
let t = Array.make (tSize+1) 0 ;;
for i=1 to tSize do
  t.(i) <- i*((3*i)-1)/2 ;
done;;
let is_penta x i =
  let j = ref (i) and res = ref false in   
  if (t.(i) > x) then
    while (!j > 0 && not !res && t.(!j) >= x) do
      if (t.(!j) = x) then res := true
      else decr j;
    done
  else
    while (!j < tSize && not !res && t.(!j) <= x) do
      if (t.(!j) = x) then res := true
      else
        incr j;
    done;
  !res
;;
let minDist = ref 1000000000 ;;
let i = ref 1 and j = ref tSize in
while !j > 1 do
  i := !j - 1 ;
  while (!i > 0) do
    let a = (t.(!i) + t.(!j))  and b = (t.(!j) - t.(!i)) in
    if ((is_penta a !j) && (is_penta b !j)) then (
      if (!minDist > b) then 
        minDist := b ;
    );
    decr i ;
  done;
  decr j ;
done;
Printf.printf "%d\n" !minDist ;;
