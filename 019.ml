let m_of_y m y = match m with
  | 0 | 2 | 4 | 6 | 7 | 9 | 11 -> 31
  | 3 | 5 | 8 | 10 -> 30
  | 1 when (y mod 4 = 0) -> 29
  | 1 -> 28
  | _ -> -10000000 ;;

let cursor = ref 1 in
let counter = ref 0 in
for y=1901 to 2000 do
  for m=0 to 11 do
    let max = (m_of_y m y) in
    for d=1 to max do
      if (d=1 && !cursor mod 7 = 6) then
        incr counter ;
      incr cursor ;
    done;
  done;
done;
!counter;;


