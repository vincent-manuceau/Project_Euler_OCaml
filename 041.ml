let is_n_pandigital s n =
  let t = Array.make 10 0 in
  for i=0 to String.length s -1 do
    let cur = (int_of_char s.[i]) - (int_of_char '0') in
    t.(cur) <- t.(cur) + 1 ;
  done;
  let res = ref true in
  for i=1 to n do
    res := !res && t.(i) = 1 ;
  done;
  for i=n+1 to 9 do
    res := !res && t.(i) = 0 ;
  done;
  res := !res && t.(0) = 0 ;
  !res ;;

let get_primes () =
  let t = Array.make 10000000 true in
  for i=2 to Array.length t -1 do
    for j=i to (Array.length t - 1)/i do
      t.(i*j) <- false;
    done;
  done;
  t.(1) <- false;
  t in
let max = ref 0 and nMax = ref 0 in
let t = get_primes () in
for i=2 to Array.length t -1 do
  if (t.(i)) then (
    let str = string_of_int i in
    for n=2 to 9 do
      if (is_n_pandigital str n) then (
        if (i > !max) then (
          max := i ;
          nMax := n;
        );
      )
    done;
  );
done;
Printf.printf "%d\n" !max ;;

