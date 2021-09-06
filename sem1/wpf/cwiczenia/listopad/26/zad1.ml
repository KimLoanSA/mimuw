module type NOSNIK = 
	sig
		type t
	end;;

module type MONOID = 
	sig 
		type t
		val op: t -> t -> t
		val e: t
	end;;


module MonoidFunkcji (N: NOSNIK): MONOID with type t = N.t -> N.t =
	struct
		type t = N.t -> N.t

		let op f g x = f( g x)
		let e = fun x -> x

	end;;

module MF = MonoidFunkcji (struct type t = int end);;
let x = MF.op (fun x -> x) (fun x -> x + 2);;
let y = x 3;;

