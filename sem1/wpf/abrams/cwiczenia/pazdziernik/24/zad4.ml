type tree = N of int * tree list

let rec impreza t =
	let N(w, l) = t
	in 
		let (slb, sl) = imprezaLD l 0 0 
			in 
				(sl, max sl (slb + w))

and imprezaLD l sb s =
	match l with 
	| [] -> (sb, s)
	| h::t -> 
		let (shb, sh) = impreza h
			in 
				imprezaLD t (sb + shb) (s + sh)
;;