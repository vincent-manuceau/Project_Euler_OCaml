let base2 n =
  let rec base2rec n l = match n with
    | 0 -> l
    | _ ->  base2rec (n/2) ((n mod 2) :: l) in
  let rec tabFill n tab l = match l with
    | [] -> ()
    | h::t -> (tab.(n) <- h ; tabFill (n+1) tab t ;) in
      
  let l = base2rec n [] in
  let t = Array.make (List.length l) 0 in
  tabFill 0 t l ;
  t ;;

let base10 n =
  let s = string_of_int n in
  let t = Array.make (String.length s) 0 in
  for i=0 to Array.length t -1 do
    t.(i) <- (int_of_char s.[i])-(int_of_char '0') ;
  done;
  t ;;

let isPal t =
  let rec test n t =
    let n_mirror = (Array.length t -1 - n) in
    if (n >= n_mirror) then true
    else if (t.(n) <> t.(n_mirror)) then false
    else test (n+1) t in
  test 0 t ;;
let counter = ref 0 in
for i=1 to 1000000 do
  let b2 = base2 i and b10 = base10 i in
  if (isPal b2) && (isPal b10) then
    (
      counter := i + !counter;
    );
done;
Printf.printf "%d\n" !counter;
