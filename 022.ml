let get () =
  let readfile () =
    let str = ref "" in
    let ic = open_in "p022_names.txt" in
    str := input_line ic;
    close_in ic ;
    !str
  in
  let s = readfile() in
  let t = Array.make 5164 "" in
  let cur = ref 1 and curName = ref 1 in
  while (!cur < String.length s) do
    let name = ref "" in
(*  Printf.printf "%c" s.[!cur] ;
    flush stdout;*)
    while(s.[!cur] <> '"') do
      name := !name ^ (String.sub s !cur 1) ;
      incr cur;
    done;
    t.(!curName) <- !name ;
    incr curName ;
    (*  Printf.printf "%s\n" !name ;
        flush stdout ;*)
    cur := !cur + 3 ;
  done ;
  t ;;
let t = get() and max = 5163 ;;
for i=1 to 5162 do
  for j=i+1 to 5163 do
    let a = t.(i) and b = t.(j) in
    if (a > b) then
      ( t.(i) <- b ; t.(j) <- a ;)
  done ;
done ;
let str_to_num s =
  let count = ref 0 in
  for i=0 to String.length s - 1 do
    count := !count + (int_of_char s.[i]) - (int_of_char 'A') +1 ;
  done;
  !count in
let total = ref 0 in
for i=1 to 5163 do
  total := !total + i*(str_to_num t.(i)) ;
done ;
Printf.printf "%d\n" !total ;;
  
