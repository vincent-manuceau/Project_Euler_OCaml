let rec sum a i = if (i = Array.length a) then 0 else (a.(i)+ sum a (i+1));;
let pow2 power =
let a = Array.make (100*50) 0 in
for i=1 to Array.length a - 1 do 
 a.(i) <- 0 ;
done ;
a.(0) <- 1 ;

let rem = ref 0 in
(*for j=0 to 99 do*)
for i=1 to power do
 let k = ref 0 in
  (*Printf.printf "rem:%d prod:%d a.(0):%d\n" !rem prod a.(0);*)
  while ((!rem > 0 || (!k < Array.length a))) do 
   (*if (a.(!k) = 0) then a.(!k) <- 1 ;*)
   let prod1 = (2*a.(!k) + !rem) in
   rem := prod1 / 10 ;

   if (!rem = 0) then
    a.(!k) <- prod1
   else ( 
    a.(!k) <- prod1 mod 10 ;   
   );
   incr k;
  done; 
done ;
a ;;
sum (pow2 1000) 0 ;;
