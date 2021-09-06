let rec reszt x =
	let rec pom s1 s2 x = 
		if( x = 0 ) then s1 - s2
		else pom ( s1 + x mod 10 ) ( s2 + ( ( x / 10 ) mod 10 ) ) ( x / 100 )
	in 
		let r = pom 0 0 x
		in
			if r > 10 then reszt r
			else if r < 0 then ( 10 - reszt ( ( -r ) - 1 ) )
			else r
;;