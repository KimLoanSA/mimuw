let wzrost l =
	let f (maxl, maxd, aktl, aktd) x =
		let (aktl2, aktd2) = 
			match aktl with
			| [] -> ([x], 1)
			| h::t -> 
				if h < x then
					(x::aktl, aktd + 1)
				else
					([x], 1) in
		if aktd2 > maxd then
			(aktl2, aktd2, aktl2, aktd2)
		else
			(maxl, maxd, aktl2, aktd2) in
	let (wyn, _, _, _) = List.fold_left f ([], 0, [], 0) l in
	List.rev wyn
;;

