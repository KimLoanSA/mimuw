let rec spr x ost =
		if x = 0 then
			true
		else if not((x mod 2) = 1 && ost = 1) then
			spr (x / 2) (x mod 2)
		else
			false

let rzadkie a =
	let rec pom licz wyn =
		if licz = 0 then
			wyn
		else if spr (licz / 2) (licz mod 2) then
			pom (licz - 1) (wyn + 1)
		else 
			pom (licz - 1) wyn

	in pom a 0
;;

