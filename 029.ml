let max = 10000 ;;
let comp a b =
  let ans = ref true in
  for i=0 to Array.length a -1 do
    ans := !ans && a.(i) = b.(i)
  done;
  if (!ans) then 0 else -1 ;;

let rec not_in a l = match l with
  | [] -> true
  | h :: t -> if (comp a h) = 0 then false else (not_in a t) ;;

let pow a b =
  let aT = Array.make 3 0 and
  t = Array.make max 0 and
  s = Array.make_matrix 3 max 0 in
  let rem = ref a and i = ref 0 in
  while ( !rem > 0) do
    aT.(!i) <- !rem mod 10 ;
    t.(!i) <- aT.(!i) ;
    rem := !rem / 10 ;
    incr i ;
  done;
  for i=2 to b do
    for j=0 to 2 do
      rem := 0 ;
      for k=0 to max-1 do
        let prod = (!rem + (aT.(j) * t.(k)))  in
        rem := prod / 10 ;
        if (k+j <= max-1) then
          s.(j).(k+j) <- prod mod 10
        else if (prod > 0) then
          (
            Printf.printf "ERROR k %d j %d prod %d\n" k j prod; 
            flush stdout ;
          )
      done;
    done;
    rem := 0;
    let sum = ref 0 in
    for k=0 to max-1 do
      sum := !rem;
      for j=0 to 2 do
        sum := !sum + s.(j).(k) ;
        s.(j).(k) <- 0;
      done;
      t.(k) <- !sum mod 10 ;
      rem := !sum / 10 ;
    done;
  done;
  t ;;

let list = ref [] ;;
for a = 2 to 100 do
  Printf.printf "\ra : %d" a;
  flush stdout ;
  for b = 2 to 100 do
    let x = pow a b in
    if (not_in x !list) then
      list := x :: !list ;
  done ;
done ;;
Printf.printf "\n%d\n" (List.length !list) ;;
