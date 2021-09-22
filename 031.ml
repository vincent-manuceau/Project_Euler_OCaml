let m = [| 100; 50 ; 20 ; 10 ; 5; 2 ; 1 |] ;;

let sum t =
  let rec aux t i =
    if (i == Array.length t) then 0
    else t.(i)*m.(i) + (aux t (i+1)) in
  aux t 0 ;;

let copy t =
  let u = Array.make (Array.length t) 0 in
  for i=0 to Array.length t -1 do
    u.(i) <- t.(i) ;
  done;
  u ;;

let disp u =
  let rec aux u i = if (i = Array.length u) then ""
    else (string_of_int u.(i))^" "^(aux u (i+1)) in
  Printf.printf "%s\n " (aux u 0) ; flush stdout ;;


let rec fill t start =
  if (start >= Array.length m) then (
    0
  )
  else (
    let counter = ref 0 in
     for i=0 to (200/m.(start)) do
       let u = copy t in
       u.(start) <- i ;
       let curSum = (sum u) in
       if curSum = 200 then (
         disp u ;
         incr counter ;
         )
       else if (curSum < 200 ) then (
         counter := !counter + (fill (copy u) (start+1)) ;
       )
     done;
     !counter ;
  )
;;

let t = Array.make 7 0 ;;
Printf.printf "%d\n" (1+(fill t 0)) ;;
