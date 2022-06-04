module type COUNTER = 
	sig 
		type counter
		val make: unit -> counter
		val inc: counter -> int
		val reset: unit -> unit
	end
;;

module MyCOUNTER: COUNTER =
	struct
		type counter = int ref * int ref
		let glob_licz = ref 0

		let make () = (ref 0, ref 0)
		let inc c =
			match c with
			| (c_val, c_licz) ->
				if !c_licz = !glob_licz then
					(c_val := !c_val + 1;
					!c_val)
				else
					(c_val := 1;
					c_licz := !glob_licz;
					!c_val)

		let reset () =
			glob_licz := !glob_licz + 1
	end
;;

