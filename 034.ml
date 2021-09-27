let frac_sum n =
  let rec frac n = if (n=0 || n=1) then 1 else n*(frac (n-1)) in
  let rec fsum l = match l with [] -> 0
                              | h::t -> (frac h) + (fsum t) in
  let l = ref [] in
  let s = string_of_int n in
  for i=0 to (String.length s)-1 do
    l := ((int_of_char s.[i])-(int_of_char '0')) :: !l ;
  done;
  (fsum !l) = n ;;
let sum = ref 0 in
for i=3 to 100000 do
  if (frac_sum i) then 
    sum := i + !sum ;
done;
Printf.printf "%d\n" !sum ;;
