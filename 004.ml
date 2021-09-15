let is_pal s l =
 let pal = ref true and i = ref 0 in
 while (!i < l && !pal) do
  pal := !pal && (s.(!i) = s.(l-1 - !i)) ;
  incr i;
 done ;
!pal ;;

let max = ref 0 ;;
let s = Array.make 6 0 ;;
for i=100 to 999 do
 for j=100 to 999 do
  let cur_len = ref 0 and cur_res = ref (i*j) in
  while (!cur_res > 0) do
   (*s.[!cur_len] <- !cur_res mod 10 ;*)
   s.(!cur_len) <- !cur_res mod 10 ;
   cur_res := !cur_res / 10 ;
   incr cur_len;
  done ;
  if (is_pal s !cur_len) then (
  Printf.printf "%d x %d = %d\n" i j (i*j) ;
  if (i*j > !max) then
   max := i*j ;
  )
 done ;
done ;;

Printf.printf "\nmax : %d\n" !max ;;
