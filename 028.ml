let max = 1001 ;;
let m = Array.make_matrix max max 0 ;;
let rec fill x y n c =
  let counter = ref c in
  let dir = ref 1 in
  let end_fill = ref false in
  if (n mod 2 = 0) then
    dir := -1 ;
  
  for i=1 to n do
    end_fill := !end_fill || x+ !dir*i > (max-1) ;
    if (not !end_fill) then (
     (* Printf.printf "y:%d x:%d <- %d\n" y (x + !dir*i) !counter ;
        flush stdout;*)
      m.(y).(x+ !dir*i) <- !counter ;
      incr counter ;
    ) ;
  done ;
  for j=1 to n do
    end_fill := !end_fill || y+ !dir*j > (max-1) ; 
    if (not !end_fill) then (  
     (* Printf.printf "y:%d x:%d <- %d\n" (y+ !dir*j) (x + !dir*n) !counter ;
        flush stdout;*)
      m.(y+ !dir*j).(x+ !dir*n) <- !counter;
      incr counter ;
    )
  done ;
  if (not !end_fill) then
    fill (x+ !dir*n) (y+ !dir*n) (n+1) !counter  
;;
m.(max/2).(max/2) <- 1 ;;
fill (max/2) (max/2) 1 2 ;;
m;;
let sum = ref 0 in
for i=0 to (max-1) do
  if (i <> max-1-i) then
    sum := !sum + m.(i).(i) + m.(i).(max-1-i) 
  else
    sum := !sum + m.(i).(i) ;
done ;
Printf.printf "Sum: %d\n" !sum ;;
