let licz li =
	rev list.fold_left 
		(fun a e -> 
			match a with
			|[] -> e
			|h::t -> (h+e)::a)

		[]
		l)
;;
