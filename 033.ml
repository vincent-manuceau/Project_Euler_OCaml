let eq n1 d1 n2 d2 =
  let i = (float_of_int n1) /. (float_of_int d1) and
  j = (float_of_int n2) /. (float_of_int d2) in
  i = j ;;

let has_common_n n d c =
  let ns = string_of_int n in
  let ds = string_of_int d in
  try (
    let ni = String.index ns c in
    let di = String.index ds c in
    (ni >= 0 && di >= 0)
  ) with | Not_found -> false
         | e -> raise (e)
;;

let strip_common_n n d c =
  let ns = string_of_int n in
  let ds = string_of_int d in
  try (
    let ni = String.index ns c in
    let di = String.index ds c in

    let nf = (String.sub ns 0 ni) ^ (String.sub ns (ni+1) ((String.length ns)-(ni+1))) in
    let df = (String.sub ds 0 di) ^ (String.sub ds (di+1) ((String.length ds)-(di+1))) in

    (int_of_string nf, int_of_string df) 
    
  ) with | e -> raise (e)
  ;;
let rec gcd a b = if (b = 0) then a else (gcd b (a mod b)) ;;
let res = ref [] ;;
let res2 = ref [] ;;

for i=10 to 99 do
  for j=i+1 to 99 do
    for k=1 to 9 do
      let c = char_of_int (k+ int_of_char '0') in
      if (has_common_n i j c) then
        (
          let (a,b) = strip_common_n i j c in
          if (eq i j a b) then
            (
              Printf.printf "%d/%d = %d/%d\n" i j a b ;
              flush stdout;

              let g = gcd a b in
              if (g = 1) then (
                res2 := b :: !res2 ;
                res :=  a :: !res ;
              )
              else(
                res2 := (b/g) :: !res2 ;
                res := (a/g) :: !res ;
              )
            )
        )
    done;
  done;
done;;
let rec prod l = match l with [] -> 1 | h::t -> h * (prod t) ;;
Printf.printf "%d\n" ((prod !res2)/(prod !res)) ;;
