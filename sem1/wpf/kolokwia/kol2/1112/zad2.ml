let cykl a =
	let odw = Array.make (Array.length a) 0
	and najlepszy = ref 0 in
	for i = 0 to Array.length a - 1 do
		if odw.(i) = 0 then
			begin
				let akt = ref 0
				and el = ref i in
				while odw.(!el) = 0 do
					odw.(!el) <- 1;
					el := a.(!el);
					akt := !akt + 1;
				done;
				if akt > najlepszy then
					najlepszy := !akt
			end
	done;
	!najlepszy
;;