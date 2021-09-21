let list = ref [] ;;
let rec sum l = match l with [] -> 0 | h::t -> h + (sum t) ;;
let pow5 a =
  let rec aux a i = if (i = 0) then 1 else a * (aux a (i-1)) in
  aux a 5 ;;
let rec finder curSum curStr depth =
  if (depth > 0) then
    for i=0 to 9 do
      let sum = curSum + pow5 i
      and str = curStr ^ string_of_int i in
      if (string_of_int sum = str) then (
        Printf.printf "%s %d\n" str sum;
        flush stdout ;
        if (sum <> 0 && sum <> 1) then
          list := sum :: !list ;
      );
      finder sum str (depth-1)
    done;;
finder 0 "" 7  ;;
Printf.printf "\nSum : %d\n" (sum !list) ;;
