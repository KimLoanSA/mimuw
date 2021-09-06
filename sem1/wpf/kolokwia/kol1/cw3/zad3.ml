let zera_silni n =
	let rec pom n pot wyn =
		if pot > n then
			wyn
		else
			pom n (pot * 5) (wyn + (n / pot))
	in
		pom n 5 0;;
		