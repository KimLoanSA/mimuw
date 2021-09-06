open List;;

let sign x =
	if x < 0 then 
		-1
	else 
		1

let prawie l =
	let f (akt_r, akt_l, czy0, max_r, max_l) x =
		match akt_l with
		| [] -> (1, x::[], x = 0, 1, x::[])
		| h::t -> 
			if x = 0 then
				if czy0 then
					if akt_r > max_r then
						(1, x::[], true, akt_r, akt_l)
					else
						(1, x::[], true, max_r, max_l)
				else
					if akt_r + 1 > max_r then
						(akt_r + 1, x::akt_l, true, akt_r + 1, x::akt_l)
					else
						(akt_r + 1, x::akt_l, true, max_r, max_l)
			else
				if czy0 then
					if h = 0 then
						if akt_r + 1 > max_r then
							(akt_r + 1, x::akt_l, true, akt_r + 1, x::akt_l)
						else
							(akt_r + 1, x::akt_l, true, max_r, max_l)
					else
						if sign h = ((sign x) * (-1)) then
							if akt_r + 1 > max_r then
								(akt_r + 1, x::akt_l, true, akt_r + 1, x::akt_l)
							else
								(akt_r + 1, x::akt_l, true, max_r, max_l)
						else
							if akt_r > max_r then
								(1, x::[], false, akt_r, akt_l)
							else
								(1, x::[], false, max_r, max_l)
				else
					if sign h = ((sign x) * (-1)) then
						if akt_r + 1 > max_r then
							(akt_r + 1, x::akt_l, false, akt_r + 1, x::akt_l)
						else
							(akt_r + 1, x::akt_l, false, max_r, max_l)
					else
						if akt_r > max_r then
							(1, x::[], false, akt_r, akt_l)
						else
							(1, x::[], false, max_r, max_l)

	in
		fold_left f (0, [], false, 0, []) l
;;











