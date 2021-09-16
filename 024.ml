let p = Array.make 3628800 "" ;;
let cursor = ref 0 ;;


let rec string_of_list l = match l with
  | [] -> ""
  | h::t -> (string_of_list t) ^ (Char.escaped (char_of_int (h+(int_of_char '0')))) ;;

let rec not_in i l = match l with
  | [] -> true
  | h::t -> if (i=h) then false else (not_in i t) ;;

let rec perm l k =
    if (k = 10) then
      (
        p.(!cursor) <- (string_of_list l);
        incr cursor ;
        (*        Printf.printf "%s\n" p.(!cursor -1) ;*)
      )
    else
      (
        for i=0 to 9 do
          if (not_in i l) then
            perm (i::l) (k+1)
        done ;
      );;

perm [] 0 ;;
Printf.printf "%s\n" p.(999999) ;;
