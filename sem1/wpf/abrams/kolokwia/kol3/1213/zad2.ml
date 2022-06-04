let kwadrat tab n m =
	let pref = Array.make_matrix (n + 1) (m + 1) 0 in
	let rec prefix tab i j =
		if i = n && j = m then ()
		else
			pref.(i).(j) <- pref.(i - 1).(j) + pref.(i).(j - 1) + tab.(i).(j);
			if j = m then
				prefix (i + 1) 1
			else
				prefix i (j + 1) in
	let rec sprawdz i j wiel wyn =
		if i = n && j = m then
			wyn
		else 
	
