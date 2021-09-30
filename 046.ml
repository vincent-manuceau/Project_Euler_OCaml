let tSize = 10000000 ;;
let t = Array.make (tSize+1) true ;;
t.(0) <- false ;
for i=2 to tSize do
  for j=2 to tSize/i do
    t.(i*j) <- false ;
  done;
done;;
let curN = ref 2 and counter_argument = ref false ;;
while not !counter_argument do
  if (!curN mod 2 <> 0 && not t.(!curN)) then (
    let curP = ref (!curN-1) and i = ref 1 and found = ref false in
    while !curP > 0 && !i < !curN && not !found do
      let curVal = !curP + 2*( !i * !i) in
      if (not t.(!curP)) then (
        i := 1 ;
        decr curP ;
      )
      else if  curVal < !curN then  
        incr i       
      else if  curVal = !curN then 
        found := true
      else (
        decr curP ;
        i := 1;
      );
    done;
    if (not !found) then (
      counter_argument := true  ;
      Printf.printf "counter argument : %d\n" !curN ;
      flush stdout ;
    );
  );
  incr curN ;
done;
