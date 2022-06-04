let eps = 0.0000000001;;

let odwrotnosc f = 
	fun y ->
		let rec szukaj l p =
			let s = (l +. p) /. 2.
			in
				if(abs_float(f(s) -. y) < eps ) then
					s
				else if f(S) > y then
					szukaj l s
				else
					szukaj s p
		in
			if abs_float y < eps then 
				0.
			else if y > 0. then
				szukaj 0. y
			else
				szukaj y 0.
;;

