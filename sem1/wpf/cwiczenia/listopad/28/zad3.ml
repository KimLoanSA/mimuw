type 'a drzewo = 
| Puste
| Wezel of 'a * 'a drzewo * 'a drzewo * 'a drzewo ref

let fastryguj d =
	let rec spacer x s =
		match x with
		| Puste -> s
		| Wezel(_, l, p, r) ->
			let nast = spacer p s
			in
				begin
					r := nast;
					spacer l x;
				end

	in 
		ignore (spacer d Puste)
;;