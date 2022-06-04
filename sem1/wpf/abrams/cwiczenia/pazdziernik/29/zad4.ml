let non p =
	fun x -> 
		not (p x)

let forall li p =
	not (sxists li (non p))