let upper_bound a x n =
	let i := 0
	and g := n - 1
	and s := 0 in
	while i < j do 
		begin
			s := (i + j + 1) / 2 in
			if a.(s) <=  then
				i := s
			else
				j := s - 1
		end
