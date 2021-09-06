let rozne t =
	let n = Array.length t - 1 in
	let pocz = ref 0
	and kon = ref n
	and wyn = ref true in
	while pocz < kon && !wyn do 
		if t.(!pocz) = t.(!kon) then
			wyn := false
		else
			if t.(!pocz) < t.(!kon) then
				kon := !kon - 1
			else
				pocz := !pocz + 1
	done;
	!wyn
;;