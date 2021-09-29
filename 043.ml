let test s =
  let p = [|2 ; 3 ; 5 ; 7 ; 11 ; 13 ; 17 |] in
  let res = ref true in
  let i = ref 0 in
  while !res && !i < Array.length p do
    let x = int_of_string (String.sub s (!i+1) 3) in
    res := !res && ((x mod p.(!i)) = 0);
    incr i ;
  done;
  !res in
let counter = ref 0 in
let rec not_in i = function [] -> true | h::t -> if (h <> i) then (not_in i t) else false in
let rec string_of_list l = match l with [] -> "" | h::t -> ((string_of_int h) ^ (string_of_list t)) in
let rec genx l =
  if (List.length l = 10) then (
    let s = string_of_list l in
    if (test s) then
      counter := !counter + (int_of_string s) ;)
  else (
    for i=0 to 9 do
      if (not_in i l) then (
        genx (i::l) ;
      )      
    done;
  ) in
genx [] ;
Printf.printf "%d\n" !counter;;

