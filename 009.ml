for c=1 to 500 do
 for a=1 to c do
  for b=a to c do
  if ((a*a + b*b = c*c) && (a + b + c = 1000)) then begin 
   Printf.printf "a:%d b:%d c:%d -> a*b*c = %d\n" a b c (a*b*c) ;
   end
  done
 done
done ;;
