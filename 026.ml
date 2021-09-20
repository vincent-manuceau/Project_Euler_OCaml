let max = 10000  in
let d = Array.make (max+1) 0 in

let process n =
  for i=0 to max do
    d.(i) <- 0 ;
  done ;
  let rem = ref 1 and cur = ref 0 in
  while (!rem > 0 && !cur < max) do
    let div = (10 * !rem) / n in
    rem := (10 * !rem) mod n ;
    d.(!cur) <- div ;
    incr cur ;    
  done in

let disp () =
  Printf.printf "\n0." ;
  for i=0 to max do
    Printf.printf "%d" d.(i) ;
  done;
  flush stdout
in

let check_rep start rep = 
  let ok = ref true and id = ref true in
  for i=0 to rep do
    if (start + i + rep > max) then ok := !ok && false
    else (
      ok := !ok && (d.(start+i) == d.(start+i+rep));
      id := !id && d.(start+i) = d.(start) && d.(start+i+rep) = d.(start) ;
    );
  done ;
  !ok && (not !id) in
let maxLen = ref 0 and maxN = ref 0 in
for i=2 to 1000 do 
  process i ;

  for k=0 to 10 do
    let j = ref 2 and b = ref false in
    while (!j < max/(k+1) && not !b) do
     (* for j=2 to max/(k+1) do*)
      let a = check_rep k !j in
      b := a ;
      if (a && !j > 10) then (
       
        (*  disp();
        Printf.printf "\n1/%d -> k= %d rep= %d\n" i k !j ;
        *)
        if (!maxLen < !j) then (
          maxLen := !j ;
          maxN := i ;
        );
         Printf.printf "\ri=%d (maxLen %d maxN %d)" i !maxLen !maxN ;  flush stdout ;
      );
      incr j;
    done ;
done;
(*  for k=999 downto 0 do
    let j=  and curLen = ref false in
    while (!j >= 0 && not !curLen) do
      (*for j=2 to 1000 do*)
      curLen := (check_rep k !j);
(*     Printf.printf "\ncheck rep %d %d = %b" k j curLen ;
       flush stdout ;*)
      if (!curLen && !j > !maxLen) then (
        maxLen := !j ;
        maxN := i ;
      );
      decr j;
    done ;
  done ; *)
done;
Printf.printf "Max rep = %d for n=%d\n" !maxLen !maxN ;
flush stdout ;;
