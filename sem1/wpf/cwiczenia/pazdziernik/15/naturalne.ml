let rec buduj n = 
	if n = 0 then []
	else n :: buduj ( n - 1 )
;;

let buduj2 n =
	let rec pom n l =
		if n = 0 then l
		else pom ( n - 1 ) ( n :: l )

	in
		pom n []
;;