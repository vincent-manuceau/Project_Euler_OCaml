let get_primes max =
  let e = Array.make (max+1) 0 in
  for i=2 to max do
    for j=i to max do
      if(i*j < max+1) then
        e.(i*j) <- 1;
    done;
  done;
  let rec p i l =
    if (i = max+1) then l
    else if (e.(i) = 0) then (p (i+1) (i::l))
    else (p (i+1) l) in
  let rec inv l acc = match l with  h::t -> (inv t (h::acc)) | [] -> acc in
  (inv ((p 1 [])) []) ;;

let rec in_list i l = match l with
      h::t -> if (h = i) then true else (in_list i t)
    | [] -> false ;;

let primes = get_primes 1000 ;;
let is_prime n = (in_list n primes) ;;
let abs n = if (n < 0) then -1*n else n ;;

let maxPrimes = ref 0 and maxA = ref 0 and maxB = ref 0 in
for bb=0 to 2000 do
  let b = bb-1000 in
  for aa=0 to 1998 do
    let a = aa-999 in
    let num_primes = ref 0 and ans = ref true and i = ref 0 in
    while (!ans) do
      let n = !i in
      let p = n*n+ a*n + b in
      ans := !ans && (is_prime p);
      if (!ans) then incr num_primes ;
      incr i;
    done ;
    if (!num_primes > !maxPrimes) then begin
        maxPrimes := !num_primes ;
        maxA := a ;
        maxB := b ;
         Printf.printf "\ra: %d b: %d cur_max_primes: %d  " !maxA !maxB !maxPrimes ;
flush stdout ;
    end;
  done;
done;
    
Printf.printf "\n\na: %d b: %d max_primes: %d  result: %d \n" !maxA !maxB !maxPrimes (!maxA * !maxB);
flush stdout ;;
