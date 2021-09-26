let results = ref [] ;;
let sum = ref 0 ;;
let rec not_in i l = match l with [] -> true
                                | h::t -> (if (h = i) then false else (not_in i t)) ;;
let is_pandigital n =
  let res = ref true in
  let rem = ref n in
  let t = Array.make 9 0 in
  while !rem > 0 do
    let a = !rem mod 10 in
    if (a = 0) then (
      res := false;
      rem := 0 ;
    )
    else if (t.(a-1) <> 0) then
      (
        res := false;
        rem := 0 ;
      )
    else(
      t.(a-1) <- 1;
      rem := !rem / 10 ;
    )
  done;
  for i=0 to 8 do
    res := !res && (t.(i) <> 0);
  done;
  !res ;;

for i=1 to 100000 do
  for j=i+1 to 100000/i do
    let prod = i*j in
    let test = int_of_string ((string_of_int i)^(string_of_int j)^(string_of_int prod)) in
    if (is_pandigital test) then (
      Printf.printf "%d x %d = %d\n" i j prod;
      flush stdout;
      if (not_in prod !results) then (
        results := prod :: !results ;
        sum := !sum + prod ;
      )
    );
  done;
done;;
Printf.printf "%d\n" !sum;
