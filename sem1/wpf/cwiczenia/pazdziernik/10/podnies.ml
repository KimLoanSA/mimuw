let square x = x * x;;

let podnies x y =
	let rec pom n potega akum =
		if n = 0 then akum
		else if n mod 2 = 1 then pom ( n / 2 ) ( square potega ) ( akum * potega )
		else pom ( n / 2 ) ( square potega ) akum

	in 
		if y = 0 then 1 
		else pom y x 1
;;
