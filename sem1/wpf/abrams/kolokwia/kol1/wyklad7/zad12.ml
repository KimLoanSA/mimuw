open List;;

let prextrema l =
	let rec pom mini maxi l acc =
		match l with
		| [] -> acc
		| h::t -> 
			if h < mini || h > maxi then
				pom (min mini h) (max maxi h) t (h::acc)
			else
				pom (min mini h) (max maxi h) t acc
	in 
		rev (pom max_int min_int l [])
;;

let prextrema2 l =
	let f (mini, maxi, acc) x =
		if x < mini || x > maxi then
			(min mini x, max maxi x, x::acc)
		else 
			(min mini x, max maxi x, acc)

	in
		fold_left f (max_int, min_int, []) l;;
