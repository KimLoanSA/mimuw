let odwr x =
	let rec pom a w = 
		if a = 0 then w
		else pom ( a / 10 ) ( w * 10 + ( a mod 10 ) )
	in pom x 0;;