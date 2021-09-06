open List;;


let slupki l =
	let mediana l =
		nth (sort compare l) ((length l) / 2)
	in let med = mediana l
		in
			fold_left (fun aku a -> aku + (a - med)) 0 l;;
