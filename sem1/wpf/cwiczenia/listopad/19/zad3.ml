open List;;

let przedzial l r =
	let t = sort compare l
	in
		let pom c op lp ik lk max_p max_c =
			match lk with
			| [] -> max_c
			| hk::tk ->
				match lp with
				| hp::tp ->
					if hp < c - r then
						pom c (ip + 1) tp ik lk max_p max_c
					else if hk > c + r then
						pom (c + 1) ip lp ik lk max_p max_c 
					else if nk - np + 1 > max_p then
						pom c ip lp (ik + 1) tk (ik - lp + 1) c
					else
						pom c ip lp (ik + 1) tk max_p max_c
		in
			match l with
			| [] -> 0
			| h::t ->
				pom h 2 l 2 t 1 h;;