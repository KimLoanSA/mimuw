let trojki l = 

	let rec pomc l a b wyn =
		match l with
			[] ->  wyn |
			h :: t -> 
				if ( h < ( a + b ) ) then pomc t a b ( ( a, b, h ) :: wyn )
				else pomc t a b wyn
	in
		let rec pomb l a wyn =
			match l with
				[] -> wyn | 
				h :: t -> pomb t a ( pomc t a h wyn )
		in 
			let rec poma l wyn = 
				match l with 
					[] -> wyn |
					h :: t -> poma t ( pomb t h wyn )
			in
				poma l []
;;