let niemalejacy t =
	let wyn = ref true in
	for i = 1 to Array.length t - 1 do 
		if abs t.(i - 1) > abs t.(i) then
			wyn := false
	done;
	!wyn
;;