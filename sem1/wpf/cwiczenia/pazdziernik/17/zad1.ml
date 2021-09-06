let lider l =
	let rec kandydat l kand licz =
		match l with
			| [] -> kand 
			| h :: t ->
				if ( kand = h ) then kandydat t kand ( licz + 1 )
				else if licz > 0 then kandydat t kand ( licz - 1  )
				else kandydat t h 1
	in
		let rec sprawdz l kand licz = 
			match  l with
				| [] -> 
					if licz > 0 then ( Some kand )
					else None
				| h :: t -> 
					if kand = h then sprawdz t kand ( licz + 1 )
					else sprawdz t kand ( licz - 1 )
		in
			sprawdz l ( kandydat l 0 0 ) 0
;;