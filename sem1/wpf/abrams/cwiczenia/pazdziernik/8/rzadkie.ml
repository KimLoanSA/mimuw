let rec rzadkie1 n =
	let rec spr x =
		if ( x mod 2 = 1 ) && ( ( x / 2 ) mod 2 = 1 ) then false
		else if x > 1 then spr ( x / 2 )
		else true

	in 
		if spr ( n + 1 ) then ( n + 1 )
		else rzadkie ( n + 1 )
;;

let rzadkie2 n = 
	let rec pom x i = 
		if ( x / ( 4 * i ) ) < 2  then x 
		else if ( ( x mod ( 4 * i ) ) / i ) = 3 then pom ( ( ( x + i ) / i ) * i ) ( i * 2 )
		else pom x ( i * 2 )

	in 
		pom ( n + 1 ) 1
;;

let rzadkie3 x = 
	let rec power a b =
		if b > 0 then power ( a * a ) ( b - 1 )
		else a
	and

	let rec duel x acc =
		if ( x mod 2 = 0 ) && ( ( x / 2 ) mod 2 = 0 ) then power ( x + 1 ) acc
		else dziel ( x / 2 ) ( acc + 1 )

	in dziel x 0 
;;
