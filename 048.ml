let pow i n =
  let x = ref 1 in
  for k=1 to i do
    x := (!x * i) mod n ;
  done;
  !x in
let digits = 10000000000 and sum = ref 0 in
for i=1 to 1000 do
  sum := (!sum + (pow i digits)) mod digits ;
done;
Printf.printf "%d\n" !sum ;;
