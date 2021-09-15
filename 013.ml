let s = "37107287533902102798797998220837590246510135740250
...
53503534226472524250874054075591789781264330331690";;



let process_matrix s =
let t = Array.make_matrix 100 50 0 in
let x = ref 0 and i = ref 0 and j = ref 0 in
while !x < String.length s do
 let num1 = (String.sub s !x 1) in
 if num1.[0] = '\n' then begin
  incr j ;  
  i := 0 ;
 end 
 else begin
  let num = (int_of_char num1.[0]  - int_of_char '0') in
  t.(!j).(49 - !i) <- num ;
  incr i; 
 end ;
 incr x ; 
done ;
t;;

let t = process_matrix s ;;
let a = Array.make (100*50) 0 ;;

for j=0 to 99 do
 for i=0 to 49 do
  let sum = a.(i)+t.(j).(i) in
  let rem = ref (sum / 10) in
  if (!rem = 0) then
   a.(i) <- sum 
  else 
  (
   let k = ref (i+1) in
   a.(i) <- (sum mod 10) ;
   while (!rem > 0) do 
    let sum1 = a.(!k) + !rem in
    rem := sum1 / 10 ;
    if (!rem = 0) then
     a.(!k) <- sum1
    else ( 
     a.(!k) <- sum1 mod 10 ;
     incr k;
    );
   done;
  );
 done ;
done ;
a;;

let print = ref false and j = ref 0 in
for i= 4999 downto 0 do
 if (not !print && a.(i) <> 0) then
  print := true ;

 if (!print && !j < 10) then
 begin
  Printf.printf "%d" a.(i);
  incr j;
 end ;
done ;;
