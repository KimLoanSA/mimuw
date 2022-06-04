let domino lista =
	let max = ref [] in
	let rec przejdz l wyn=
		match l with
		| [] -> ()
		| h::t -> backtrack h wyn

	and backtrack  wyn =
		match wyn with 
		| [] -> 
		| (pop,_)::_ ->
			if pop = 0 then
				(backtrack t ((a,b)::wyn); backtrack t ((b,a)::wyn))
			else if a = pop && b = pop then
				(backtrack t ((a,b)::wyn); backtrack t ((b,a)::wyn))
			else if a = pop then
				backtrack t ((a,b)::wyn)
			else if b = pop then
				backtrack t ((b,a)::wyn)
			else
				if List.length wyn > List.length !max then
					max := wyn in 
	przejdz lista [];
	!max
;;



