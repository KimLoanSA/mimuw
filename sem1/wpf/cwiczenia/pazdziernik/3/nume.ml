let rec nume x = 
	let rec sum a w = 
		if a = 0 then w 
		else sum ( a / 10 ) ( w + a mod 10 )
	in 
		if x < 10 then 
			if ( x = 0 || x = 9 ) then true
			else false
		else nume ( sum x 0 )
;;

