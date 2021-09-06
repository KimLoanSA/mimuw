let podzial l =
	let rec pom pref l acc last =
		match l, pref with
		| [], _ -> acc
		| h::t, [] ->  pom h t acc h
		| h::t, c -> 
			if last = h then
				pom h t (c::acc) h
			else
				pom (h::c) t acc h

	in
		pom [] l []
;;