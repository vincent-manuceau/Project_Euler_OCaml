let get_primes () =
  let tSize = 10000 in
  let t = Array.make (tSize+1) true in t.(0) <- false ; t.(1) <- false;
  for i=2 to tSize do for j=2 to tSize/i do t.(i*j) <- false ; done; done;
  let counter = ref 0 in
  for i=2 to tSize do if t.(i) && String.length (string_of_int i) = 4 then incr counter; done;
  let u = Array.make (!counter) 0 and curs = ref 0 in
  for i=1 to tSize do
    if (t.(i) && String.length (string_of_int i) = 4) then (u.(!curs) <- i ; incr curs ;)
  done; u ;;
let isPerm a b c =
  let t = Array.make_matrix 3 10 0 and res = ref true in
  let aS = string_of_int a and bS = string_of_int b and cS = string_of_int c in
  for i=0 to String.length aS-1 do
    t.(0).(int_of_char aS.[i] - int_of_char '0') <- 1 + t.(0).(int_of_char aS.[i] - int_of_char '0') ;
    t.(1).(int_of_char bS.[i] - int_of_char '0') <- 1 + t.(1).(int_of_char bS.[i] - int_of_char '0') ;
    t.(2).(int_of_char cS.[i] - int_of_char '0') <- 1 + t.(2).(int_of_char cS.[i] - int_of_char '0') ;
  done;
  for i=0 to 9 do
    res := !res && (t.(0).(i) = t.(1).(i) && t.(0).(i) = t.(2).(i))
  done; !res ;;
let p = get_primes() and counter = ref 0 ;;
for i=0 to Array.length p -3 do
  for j=i+1 to Array.length p -2 do
    for k=j+1 to Array.length p -1 do
      if (isPerm p.(i) p.(j) p.(k) && (p.(j)-p.(i) = p.(k)-p.(j))) then (
        Printf.printf "%d%d%d\n" p.(i) p.(j) p.(k) ;
        flush stdout ; incr counter ;
        if (!counter = 2) then (exit 0 ;) 
      )
    done;
  done;
done;;
