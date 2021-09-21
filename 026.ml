let max = 100000  ;;
let d = Array.make (max+1) 0 ;;

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
  done ;;

let find_end () =
  let cur = ref max in
  while (!cur > 0 && d.(!cur) = 0) do
    decr cur ;
  done ;
  !cur ;;

let disp () =
  let cur_max = find_end() in

  Printf.printf "\n0." ;
  let i = ref 0 in
  while (!i <= cur_max) do
    Printf.printf "%d" d.(!i) ;
    incr i;
  done;
  print_newline();
  flush stdout
;;

let check start rep =
  let ans = ref true and i = ref 0 and j = ref 0 in
  while(!ans && !j < rep) do
    i := 0 ;
    while (!ans && (start+ !i*rep + !j) < max-1) do
      ans := !ans && (d.(start + !j) = d.(start + !i*rep + !j)) ;
      incr i ;
    done ;
    incr j;
  done;      
  !ans;
;;


let test_rep () =
  let ans = ref false and start = ref 0
  and cycle = ref 0 and cur_max = find_end() in
  if (cur_max = max - 1) then (
    (*ans := true;*)
    let found_cycle = ref false and starter = ref 0
    and num_cycles = ref 1 in
    while (not !found_cycle && !starter < 500) do
      while( not !found_cycle && !num_cycles < 10000) do
        found_cycle := (check !starter !num_cycles);
        if (not !found_cycle) then
          incr num_cycles
        else begin          
          cycle := !num_cycles ;
          ans := true;
          start := !starter ;
        end;
(*        Printf.printf "s:%d c:%d\n" !starter !num_cycles;
          flush stdout ; *)
      done;
      incr starter ;
      num_cycles := 1 ;
    done ;
  );
  (!start, !cycle, !ans) ;
;;

let maxCycle = ref 0 and maxStart = ref 0 and maxI = ref 0 in 
for i=2 to 1000 do
  process i ;
  let (start, cycle, is_cycle) = test_rep() in
  if (is_cycle) then begin
    Printf.printf "i: %d -> %d %d %b\n" i start cycle is_cycle ;
    flush stdout ;
    if (cycle > !maxCycle) then(
      maxCycle := cycle;
      maxStart := start;
      maxI := i;
    );
  end;
done;
Printf.printf ">> Max # Cycles: %d for 1/%d d=%d  start: %d\n" !maxCycle !maxI !maxI !maxStart ;;
