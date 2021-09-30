let get_primes () =
  let tSize = 1000000 in
  let t = Array.make (tSize+1) true in
  t.(0) <- false ; t.(1) <- false;
  for i=2 to tSize do
    for j=2 to tSize/i do
      t.(i*j) <- false ;
    done;
  done;
  let counter = ref 0 in
  for i=2 to tSize do
    if t.(i) then incr counter;
  done;
  let u = Array.make (!counter) 0 and curs = ref 0 in
  for i=1 to tSize do
    if (t.(i)) then (
      u.(!curs) <- i ;
      incr curs ;
    )
  done;
  u ;;
let factor n p = 
  let t = Array.make (Array.length p) 0 in
  let i = ref n and j = ref 0 in
  while ( !i > 0 && p.(!j) < n ) do
    if (!i mod p.(!j) = 0) then (
      t.(!j) <- t.(!j) + 1;      
      i := !i / p.(!j) ; 
    )
    else incr j ;
  done;
  t ;;
let check_factors a b c d =
  let i= ref 0 and test = ref true in
  let aCount = ref 0 and bCount = ref 0 and cCount = ref 0 and dCount = ref 0 in
  while ( !i < Array.length a - 1 && !test) do
    test := !test && (
        (a.(!i) = 0 || (a.(!i) <> b.(!i) && a.(!i) <> c.(!i) && a.(!i) <> d.(!i))) &&
        (b.(!i) = 0 || (b.(!i) <> a.(!i) && b.(!i) <> c.(!i) && b.(!i) <> d.(!i))) &&
        (c.(!i) = 0 || (c.(!i) <> a.(!i) && c.(!i) <> b.(!i) && c.(!i) <> d.(!i))) &&
        (d.(!i) = 0 || (d.(!i) <> a.(!i) && d.(!i) <> b.(!i) && d.(!i) <> c.(!i)))        
      ) ;
    if (a.(!i) > 0) then incr aCount;
    if (b.(!i) > 0) then incr bCount;
    if (c.(!i) > 0) then incr cCount;
    if (d.(!i) > 0) then incr dCount;
    incr i;
  done;
  !test && (!aCount=4 && !bCount=4 && !cCount=4 && !dCount=4) ;;
let check_quad i p =
  check_factors (factor i p) (factor (i+1) p) (factor (i+2) p) (factor (i+3) p) ;;
let p = get_primes() ;;
let i = ref 0 in
while not (check_quad !i p) do
  incr i ;
done;
Printf.printf "%d\n" !i ;;
