let sqrt x = 
	let rec pom d w =
		if w > x then ( d - 1 )
		else pom ( d + 1 ) ( w + 2 * d + 1 )
	in pom 0 0;;
