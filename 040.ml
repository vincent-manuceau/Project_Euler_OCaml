let c = ref 0 in
let prod = ref 1 in
for i=1 to 1000000 do
  let s = string_of_int i in
  let x = ref 0 in
  while !x < String.length s do
    incr c;    
    if (!c = 1 || !c = 100 || !c = 1000 || !c = 10000 || !c = 100000 || !c = 1000000) then 
      prod := !prod * (int_of_string (String.sub s !x 1)) ;
    incr x ;
  done;
done;
Printf.printf "%d\n" !prod;;
