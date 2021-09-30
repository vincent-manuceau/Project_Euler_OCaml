let tri n = n*(n+1)/2 and pen n = n*(3*n - 1)/2 and hex n = n*(2*n - 1) ;;
let t = ref 286 and p = ref 165 and h = ref 143 ;;
while (not (tri !t = pen !p && tri !t = hex !h)) do
       if (tri !t >= pen !p && pen !p >= hex !h) then incr h
  else if (pen !p >= tri !t && tri !t >= hex !h) then incr h
  else if (pen !p >= hex !h && hex !h >= tri !t) then incr t
  else if (hex !h >= pen !p && pen !p >= tri !t) then incr t
  else if (hex !h >= tri !t && tri !t >= pen !p) then incr p
  else if (tri !t >= hex !h && hex !h >= pen !p) then incr p;
done;
Printf.printf "%d\n" (tri !t) ;;
 
