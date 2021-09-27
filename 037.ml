let get_primes () =
  let t = Array.make 1000000 true in
  for i=2 to Array.length t -1 do
    for j=i to (Array.length t - 1)/i do
      t.(i*j) <- false;
    done;
  done;
  t.(1) <- false;
  t ;;

let truncate n =
  let s = string_of_int n in
  let l = ref [] and left = ref 0 and right = ref 0 in
  let len = String.length s in
  for i=0 to len-1 do
    let s1 = int_of_string (String.sub s i (len-i)) in
    let s2 = int_of_string (String.sub s 0 (i+1)) in
    if s1 <> n then (l := s1 :: !l ; incr left;);
    if s2 <> n then (l := s2 :: !l ; incr right);
  done;
  ((!left + !right) = 2*(len-1), !l) ;;
    
let rec check tab l = match l with [] -> true | h :: t when tab.(h) -> (check tab t) | _ -> false ;;

let p = get_primes() ;;
let counter = ref 0 ;;
for i=11 to Array.length p -1 do
  if p.(i) then (
    let (test,tr) = truncate i in
    if test && (check p tr) then (
      Printf.printf "%d\n" i ;
      flush stdout ;
      counter := i + !counter ;
    );
  );
done;
Printf.printf "%d\n" !counter ;;
