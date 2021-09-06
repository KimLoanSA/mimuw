type 'a drzewo = P | W of 'a * 'a drzewo list

let glebokosc t =
	let rec glebokoscD t =
		match t with 
		| P -> -1
		| W (w, l) -> 
			(glebokoscLD l (-1) + 1)
	and glebokoscLD l wyn =
		match l with
		| [] -> wyn
		| h::t -> 
			glebokoscLD t (max wyn (glebokoscD h))
	in 
		glebokoscD t
;;

