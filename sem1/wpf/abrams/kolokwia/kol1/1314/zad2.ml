open List;;

let fit c l =
	let rec pom ll rl n res =
		if n = 0 then
			res
		else 	
			let dif = (hd ll) + (hd rl) + c
			in
				if dif > 0 then
					pom ll (tl rl) (n - 1) (min res dif)
				else
					pom (tl ll) rl (n - 1) (min res (abs dif))
	in
		pom l (rev l) (length l) max_int
;;
