
let is_pandigital s =
  let t = Array.make 10 0 in
  for i=0 to String.length s -1 do
    let cur = (int_of_char s.[i]) - (int_of_char '0') in
    t.(cur) <- t.(cur) + 1 ;
  done;
  let res = ref true in
  for i=1 to 9 do
    res := !res && t.(i) = 1 ;
  done;
  !res ;;
let max = ref 0 ;;               
for i=1 to 100000 do
  let n = ref 2 and go = ref true and s = ref (string_of_int i) in
  while (!go) do
    s := !s ^ (string_of_int (!n * i));
    incr n ;
    go := (String.length !s < 9);
  done;
  if (String.length !s = 9) && (is_pandigital !s) then (
    if (int_of_string !s) > !max then max := (int_of_string !s) ;
  ) ;
done ;
Printf.printf "%d\n" !max ;;
