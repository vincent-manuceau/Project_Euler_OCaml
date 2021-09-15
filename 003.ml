let max = 600851475143 ;;
let cur_n = ref max ;;

let i = ref 2 ;;
while !i < !cur_n do
 if ((!cur_n mod !i) = 0) then (
  cur_n := !cur_n / !i ;
  Printf.printf "%d\n" !cur_n;
 );
 incr i ;
done ;

Printf.printf "%d\n" !cur_n ;;
