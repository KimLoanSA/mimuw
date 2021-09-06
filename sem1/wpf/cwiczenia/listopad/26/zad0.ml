module type MONOID = 
	sig 
		type t
		val op: t -> t -> t
		val e: t
	end;;



module MonoidLiczbCalkowitych : MONOID with type t = int =
	struct
		type t = int
		let op = (+)
		let e = 0
	end;;

module M = MonoidLiczbCalkowitych;;
let trzy = M.op M.e 3;;


module MonolidFunkcji : MONOID with type t = int -> int =
	struct
		type t = int -> int
		let op = fun f g -> fun x -> f( g x )
		let e = fun x -> x
	end;;