let get_primes () =
  let t = Array.make 1000000 true in
  for i=2 to Array.length t -1 do
    for j=i to (Array.length t - 1)/i do
      t.(i*j) <- false;
    done;
  done;
  t.(2) <- true;
  t.(1) <- true;
  t ;;
let get_circulars n =
  let l = ref [] in
  let s = string_of_int n in
  let len = String.length s in
  for i=0 to len-1 do
    let sub =(String.sub s i (len-i))^(String.sub s 0 (i)) in
    if (int_of_string sub) <> n then
      l := (int_of_string sub) :: !l ;
  done;
  l := n :: !l ;
  !l;;
let rec check l tab = match l with [] -> true
                               | h::t when (tab.(h)) -> (check t tab)
                               | _ -> false ;;
let rec void_circulars l tab = match l with [] -> ()
                               | h::t -> ( tab.(h) <- false; void_circulars t tab);;
let t = (get_primes()) in
let counter = ref 0 in
for i=2 to Array.length t -1 do
  if (t.(i)) then(
    let l = get_circulars i in
    if (check l t) then(
      (void_circulars l t) ;
      counter := !counter + (List.length l) ;
    );
  )
done;
Printf.printf "%d\n" !counter ;;
