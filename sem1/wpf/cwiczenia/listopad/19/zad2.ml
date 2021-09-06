open List;;

let elementy lx ly =
	let comp (ax, ay) (bx, by) =
		compare ax bx
	in let ly2 = sort comp (fst (fold_left (fun (la, ind) x -> ((x,ind)::la, ind + 1)) ([],1) ly))
		in let rec wybierz lx ind ly2 wyn =
			match ly2 with
			| [] -> wyn
			| (hy,hi)::t ->
				if hy = ind then
					wybierz lx ind t ((hi, hd lx)::wyn)
				else
					wybierz (tl lx) (ind + 1) ly2 wyn

			in let lwyn2 = sort comp (wybierz ly 1 ly2 [])
				in rev (fold_left (fun aku (ax, ay) -> ay::aku) [] lwyn2)
