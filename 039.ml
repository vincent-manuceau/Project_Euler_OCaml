let sqrt n =
  let i = ref 0 in
  while (!i * !i < n) do
    incr i ;
  done;
  if (!i * !i = n) then !i
  else -1 ;;
let lStr a b c =
  let t = Array.make 3 0 in
  t.(0) <- a ; t.(1) <- b ; t.(2) <- c ;
  for i=0 to 1 do
    for j=i+1 to 2 do
      if (t.(i) > t.(j)) then
        (let rem = t.(j) in
         t.(j) <- t.(i);
         t.(i) <- rem ;)
    done;
  done;
  (string_of_int t.(0))^" "^(string_of_int t.(1))^" "^(string_of_int t.(2))
;;
let rec not_in s l = match l with [] -> true
                            | h::t when h=s -> false
                            | h::t -> not_in s t ;; 
let tri p =
  let l = ref [] in
  for a = 1 to p/2 do
    for b = a+1 to p/2 do
      let c = sqrt (a*a + b*b) in
      if (c > 0) then 
        if a+b+c = p then 
          let s = lStr a b c in  if (not_in s !l) then l := s :: !l ;
    done;
  done;
!l ;;
let max = ref 0 and iMax = ref 0 in
for i=120 to 1000 do
  let cur = List.length (tri i) in
  if (cur > !max) then (max := cur ; iMax := i ;);done;
Printf.printf "%d\n" !iMax ;;
