module type POLGRUPA =
	sig
		type t
		val operacja: t -> t -> t
end;;

module type MONOID =
	sig
		include POLGRUPA

		val e: t
end;;

module type GRUPA =
	sig
		include monoid

		val p: t (* odwrotny *)
end;;

module MonoidCalkowity:
	MONOID with type t = int

	=struct
		type t = int
		let operacja = (+)
		let e = 0
end;;

module MonoidFunkcje:
	MONOID with type t = int -> int

	=struct
		type t = int -> int
		let operacja f g = function x -> f (g x)
		let e = function x -> x

end;;
