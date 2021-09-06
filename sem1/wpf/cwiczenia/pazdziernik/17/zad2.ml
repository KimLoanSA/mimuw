let palindrom l =
	let rec szukaj l1 l2 wyn =
		match  l1 with
		| [] -> wyn
		| h1 :: t1 -> palindrom t1 ( h1 :: l2 ) ( max( spr l1 l2 0, spr t1 l2 0, wyn ) )
	and
		 spr l1 l2 z =
		 	match l1, l2 with
		 	| [], _ -> ( 2 * z )
		 	| _, [] -> ( 2 * z )
		 	| h1 :: t1, h2 :: t2 -> 
		 		if h1 = h1 then spr t1 t2 ( z + 1 )
		 		else 2 * z
	in
		palindrom l [] 0
;;