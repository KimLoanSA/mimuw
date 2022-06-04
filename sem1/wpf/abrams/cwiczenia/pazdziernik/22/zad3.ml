type 'a tree = Node of 'a tree * 'a * 'a tree | Leaf;;

let ultraleft t =
	let rec pom t = 
		match t with
		| Leaf -> (true, 1, 1)
		| Node (l, _, p) ->
			let (lb, lmin, lmax) = pom l
			and (pb, pmin, pmax) = pom p
			in
				((lb && pb && (lmin >= pmax)), pmin + 1, lmax + 1)

	in 
		let (a,_,_) = pom t
		in a
;;

let ultraleft2 t = 
	let rec pom t gl ost = 
		match t with
		| Leaf ->  (gl <= ost, gl)
		| Node (l, _, p) -> 
			let (lb, lgl) = pom l (gl + 1) ost
			in
				if lb = false then
					(false, 0)
				else 
					pom p ( gl + 1 ) lgl
	in
		let (a, _) = pom t 0 max_int
		in a
;;