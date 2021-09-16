let rec sum a i = if (i = Array.length a) then 0 else a.(i) + (sum a (i+1)) in                       
let a = Array.make 200 0 in
a.(0) <- 1 ;
for i=1 to 100 do
  let cur = ref 0 and rem = ref 0 in
  while(!cur < 200) do
    let prod = !rem + i*a.(!cur) in
    rem := prod/10 ;
    a.(!cur) <- prod mod 10 ;
    incr cur ;
  done ;
done;
sum a 0 ;;
