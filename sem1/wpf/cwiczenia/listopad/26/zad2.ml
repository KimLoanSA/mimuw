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

module type MONOID_Z_POTEGOWANIEM =
	sig 
		type t
		val op: t -> t -> t
		val e : t
		val pot: int -> t -> t
	end;;

module MonoidZPotegowaniemFunktor (M: MONOID): MONOID_Z_POTEGOWANIEM with type t = M.t =
	struct
		include M

		let rec pot n a =
			if n = 0 then
				e
			else
				op a (pot (n - 1) a)
	end;;



module MonoidFunkcji (N: NOSNIK): MONOID with type t = N.t -> N.t =
	struct
		type t = N.t -> N.t

		let op f g x = f( g x)
		let e = fun x -> x

	end;;

module PotF = MonoidZPotegowaniemFunktor (MonoidFunkcji (struct type t = float end));;
let f = PotF.pot 3 (fun x -> 2. *. x +. 1.);;
let x = f 1;;