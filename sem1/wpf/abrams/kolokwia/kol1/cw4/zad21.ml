open List;;

let krazki rurka kr =
	let f acc x =
		match acc with
		| [] -> x::[]
		| h::t ->
			if h > x then
				x::acc
			else
				h::acc
	in 
		let mini = fold_left f [] rurka
		in
			let rec spr l rur poz =
				match l, rur with
				| [], _ -> poz
				| _, [] -> poz + 1
				| hl::tl, hr::tr ->
					if hl >= hr then
						spr tl tr (poz - 1)
					else
						spr tl rur (poz -1)

			in spr mini kr (length rurka)



;;
