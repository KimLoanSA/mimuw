open Array;;
let cykl t =
	let odw = make (length t) false
	and wyn = ref 0
	in 
		for j = 0 to length t - 1 do 
			if not odw.(j) then 
				begin
					odw.(j) <- true;
					let dl = ref 1 and k = ref t.(j)
					in
						begin
							while !k <> j do
								odw.(!k) <- true;
								incr dl;
								k := t.(!k);
							done;
							if !wyn < !dl then
								wyn := !dl;
						end;
				end;
		done;
		!wyn;
;;