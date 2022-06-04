let parz n =
	let rec pom a d =
		if ( a mod 2 = 0 ) then pom ( a / 2 ) ( d + 1 )
		else d

	in
		if( n = 0 ) then -1
		else pom n 0
;;
