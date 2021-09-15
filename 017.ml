let p n = match n with
	| 1 -> "one"
	| 2 -> "two"
	| 3 -> "three"
	| 4 -> "four"
	| 5 -> "five"
	| 6 -> "six"
	| 7 -> "seven"
	| 8 -> "eight"
	| 9 -> "nine"
	| 10 -> "ten"
	| 11 -> "eleven"
	| 12 -> "twelve"
	| 13 -> "thirteen"
	| 14 -> "fourteen"
	| 15 -> "fifteen"
	| 16 -> "sixteen"
	| 17 -> "seventeen"
	| 18 -> "eighteen"
	| 19 -> "nineteen"
	| 20 -> "twenty"
	| 30 -> "thirty"
	| 40 -> "forty"
	| 50 -> "fifty"
	| 60 -> "sixty"
	| 70 -> "seventy"
	| 80 -> "eighty"
	| 90 -> "ninety"
	| _ -> "ERROR" ;;


let rec t n s =
	if (n / 1000 > 0) then "onethousand"
	else if (n / 100 > 0) then
	(
		let hun = (100 * (n/100)) in
		if (n-hun > 0) then (t (n-hun) ((p (n/100))^"hundredand"))
		else ((p (n/100))^"hundred")
	)
	else if (n/10 > 0) then
	(
		let dec = (10 * (n/10)) in
		if (n>= 10 && n < 20) then (s^(p n))
		else if (n-dec > 0) then (t (n-dec) (s^(p (dec))))
		else (s^(p (dec)))
	)
	else if (n > 0) then
	s^(p n) 
	else
	s
;;


let l n = String.length (t n "");;
let sum = ref 0 ;;
for i=1 to 1000 do 
(*	Printf.printf "%s\n" (t i ""); *)
	sum := !sum + (l i);
done;
Printf.printf "%d\n" !sum;;