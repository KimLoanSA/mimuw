let koduj a b =
	let rec dlugosc x d = 
		if x = 0 then d
		else dlugosc ( x / 10 ) ( d * 10 )
	in 
		( a * dlugosc b 1 ) * 10 + b
;;

let odwr x =
	let rec pom a w = 
		if a = 0 then w
		else pom ( a / 10 ) ( w * 10 + ( a mod 10 ) )
	in pom x 0
;;

let dru a =
	let rec pom a w =
		if ( a mod 10 ) = 0 then w
		else pom ( a / 10 ) ( w * 10 + a mod 10 )


	in
		odwr ( pom a 0 )
;;

let pier a =
	let rec dlugosc x d = 
		if x = 0 then d
		else dlugosc ( x / 10 ) ( d * 10 )
	in
		( a - dru a ) / ( ( dlugosc ( dru a ) 1 ) * 10 )
;;