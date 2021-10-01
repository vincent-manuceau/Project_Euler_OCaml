let get_primes () =
  let tSize = 1000000 in
  let t = Array.make (tSize+1) true in
  t.(0) <- false ; t.(1) <- false;
  for i=2 to tSize do for j=2 to tSize/i do t.(i*j) <- false ; done; done;
  t ;;
let p = get_primes() and sum = ref 0 ;; let i = ref 0 and j = ref (Array.length p-1) ;;
while !sum + !i < 1000000 do
  if (p.(!i)) then sum := !sum + !i ;
  incr i;
done;
j := !i -1 ;
Printf.printf "%d %d\n" !sum !j ;
let k = ref 2 and rem = ref 0 in
while ( !j >=0 && !sum >= 0 ) do
  k := 2 ; rem := 0;
  while (!k < !j && !sum >0) do
    if (p.(!sum)) then (
        Printf.printf "%d = %d %d\n" !sum !k !j ; flush stdout ;
        k := -1 ; j := -1 ;      
      );
    if !k >= 0 && p.(!k) then (
      sum := !sum - !k ;
      rem := !rem + !k ;
    );
    incr k ;
  done;
  sum := !sum + !rem ;
  decr j;
done;;
