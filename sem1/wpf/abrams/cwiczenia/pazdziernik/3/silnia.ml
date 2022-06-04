let prime n =
	let rec pom d =
		if ( d * d ) > n then true
		else if ( n mod d ) = 0 then false
		else pom ( d + 1 )
	in pom 2;;
	