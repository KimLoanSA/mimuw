module type FIND_UNION =
	sig 
		type 'a set

		val make_set: 'a -> 'a set
		val find: 'a set -> 'a
		val qsuivalent: 'a set -> 'a set -> bool
		val union: 'a set -> 'a set -> unit
		val elemnts: 'a set -> 'a lsit
		val n_of_sets: unit -> int
	end
;;

let kolory n l =
	let module FU = Find_Union_Functor (struct end)
	in
		let x = Array.make n None
		and t = Array.make n None
		in let pom l = 
			match l with
			| [] -> TU.n_of_sets ()
			| (xh,yh)::t ->
				let kol = FU.make_set (xh,yh)
				in
					begin
						if x.(xh) = None then
							x.(xh) <- Some kol
						else 
							union x.(xh) kol;

						if y.(yh) = None then
							y.(yh) <- Some kol
						else 
							union y.(yh) kol;

						pom t
					end