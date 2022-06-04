let wzrost li =
	List.fold_right (fun h (lmax, dmax, lakt, dakt) -> 
						if lakt = [] then
							(h::[], 1, h::[], 1)
						else if h <= List.hd lakt then
							if dakt + 1 > dmax then
								(h::lakt, dakt + 1, h::lakt, dakt + 1)
							else 
								(lmax, dmax, h::lakt, dakt + 1)
						else
							(lmax, dmax, h::[], 1)) li ([],0,[],0)
;;
					