(* version 1 *)
let paths n =
 let res = ref 1 in
 for i=1 to n do
  res := !res * (n+i)/i
 done;
 !res in
paths 20 ;;

(* version 2 *)
let len = 20 in
let m = Array.make_matrix (len+1) (len+1) 0 in
let rec find_path x y =
 if (x = 0 || y = 0) then 1
 else 
  begin
   if m.(x).(y) > 0 then m.(x).(y)
   else begin
   let res = ((find_path (x-1) y) + (find_path x (y-1))) in
   m.(x).(y) <- m.(x).(y) + res;
   res 
   end ;  
  end in
Printf.printf "%d\n" (find_path 20 20);;
