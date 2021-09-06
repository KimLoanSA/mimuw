module type PORZADEK_LINIOWY =
	sig 
		type t 
		val porownaj: t -> t -> bool
	end;;

module PorzadekLiniowyFunktor (A: PORZADEK_LINIOWY) (B: PORZADEK_LINIOWY): PORZADEK_LINIOWY with type t = A.t * B.t =
	struct
		type t = A.t * B.t

		let porownaj (a1, b1) (a2, b2) =
			if A.porownaj a1 a2 then
				if A.porownaj a2 a1 then 
					B.porownaj b1 b2
				else 
					true
			else
				false
	end;;