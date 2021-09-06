open List;;

let prefiksy l =
	let rec sumuj l suma =
		match l with
		| [] -> suma
		| h::t -> sumuj t (suma + h)

	and pom l pref suma wyn =
		match l with
		| [] -> 
			if suma > 0 then 
				pref::wyn
			else 
				wyn
		| h::t -> 
			if suma > 0 then
				pom t (tl pref) (suma - h) (pref::wyn)
			else
				pom t (tl pref) (suma - h) wyn

				
	in pom (rev l) (rev l) (sumuj l 0) []
;;
