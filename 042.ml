let get () =
  let readfile () =
    let str = ref "" in
    let ic = open_in "p042_words.txt" in
    str := input_line ic;
    close_in ic ;
    !str
  in
  let s = readfile() in
  let l = ref [] in
  let cur = ref 1 and curName = ref 1 in
  while (!cur < String.length s) do
    let name = ref "" in
    while(s.[!cur] <> '"') do
      name := !name ^ (String.sub s !cur 1) ;
      incr cur;
    done;
    l := !name :: !l ;
    incr curName ;
    cur := !cur + 3 ;
  done ;
  !l ;;
let tri = Array.make 50001 0 ;;
for i=1 to 50000 do
  tri.(i) <- i*(i+1)/2 ;
done;;
let w = get() ;;
let char_val c = int_of_char c - int_of_char 'A' +1 ;;
let is_tri s t =
  let sum = ref 0 in
  for i=0 to String.length s -1 do
    sum := !sum + (char_val s.[i]) ;
  done;
  let i = ref 0 and ok = ref false in
  while (!i < 50000 && not !ok) do
    ok := (t.(!i) = !sum) ;
    incr i
  done;
  if (!i >= 50000) then false
  else !ok ;;
let rec proc res l = match l with
    [] -> res
  | h::t ->
    (
      if (is_tri h tri) then (
        Printf.printf "%s\n" h ; flush stdout ; proc (res+1) t 
      )
      else (
        Printf.printf "%s\n" h ; flush stdout ; proc (res) t
      )
    )
;;
Printf.printf "%d\n" (proc 0 w) ;;
