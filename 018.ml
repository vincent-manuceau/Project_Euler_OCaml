let s = "75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23";;

let string_to_array s =
let m = Array.make_matrix 15 15 0 and 
x = ref (-1) and i = ref 0 and j = ref 0 in
while (!x < String.length s) do 
	incr x ;
	let num1 = (String.sub s !x 2) in
	let num = 10 * (int_of_char num1.[0]  - int_of_char '0') + 
					(int_of_char num1.[1]  - int_of_char '0') in
	m.(!j).(!i) <- num ;
	x := !x +2 ;
	if (!x < String.length s) then(
		let sp = (String.sub s (!x) 1) in
		if (sp.[0] = '\n') then (
			incr j ;
			i := 0 ;
		)
		else 
			incr i ;
	)
done;
m ;;

let max a b = if a >= b then a else b ;;

let m = string_to_array s ;;
let n = Array.make_matrix 15 15 0;;
for i=0 to 14 do
	n.(14).(i) <- m.(14).(i) ;
done;

for j= 13 downto 0 do
	for i=j downto 0 do
		n.(j).(i) <- m.(j).(i) + (max (n.(j+1).(i)) (n.(j+1).(i+1))) ;
	done ;
done;
Printf.printf "%d\n" n.(0).(0) ;;