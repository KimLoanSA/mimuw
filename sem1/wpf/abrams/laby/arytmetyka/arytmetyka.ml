(* Arytmetyka przybliżonych wartości *)
(* Marcin Abramowicz *)
(* code review: Michał Niedziółka *)

(*****************************************************************************************************************************************************)
(* typ *)
(* i) dopelnienie = false przedzial: [lewy; prawy], lewy i prawy moga byc float lub (neg)infinity *) 
(* ii) dopelnienie = true przedzial: (-oo; lewy] U [prawy; +oo), lewy i prawy to float *)
(* iii) jezeli lewy > prawy to przedzial symbolizuje nan *)

type wartosc = {lewy : float; prawy : float; dopelnienie : bool}

(*****************************************************************************************************************************************************)
(* funkcje pomocnicze *)

let moj_nan = 																			(* nan zgodnie z iii) *)
	{lewy = 1.; prawy = -.1.; dopelnienie = false}


let moja_niesk = 																		(* (-oo;+oo) *)
	{lewy = neg_infinity; prawy = infinity; dopelnienie = false}



let jest_nan w = 																		(* sprawdzanie czy przedzial = nan, zgodnie z iii) *)
	(w.lewy = 1. && w.prawy = -.1.)


let negacja w =																			(* zwraca element przeciwny do w: -[a;b] = [-b;-a] *)
	{lewy = -.w.prawy; prawy = -.w.lewy; dopelnienie = w.dopelnienie}


let abs_zero x =																		(* naprawianie -0 *)
	if x = ( -.0.) then
		abs_float x
	else
		x


let czy_zero w =																		(* sprawdzanie czy wartosc rowna sie [0;0] *)
	((abs_zero w.lewy = 0.) && (abs_zero w.prawy = 0.))


let czy_nies w =																		(* sprawdzanie czy przedzial zawiera jakas nieskonczonosc *)
	((abs_float w.lewy = infinity) || (abs_float w.prawy = infinity) || (w.dopelnienie = true))

let czy_nies_spoj w =																	(* sprawdzanie czy przedzial to (-oo;+oo) *)
	((w.lewy = neg_infinity) && (w.prawy = infinity))


let min4 a b c d =																		(* zwraca mina z 4 wartosci *)
	min (min a b) (min c d) 


let max4 a b c d =																		(* zwraca maxa z 4 wartosci *)
	max (max a b) (max c d)


let wartosc_od_do_dop x y =																(* tworzy (-oo;x] U [y;+oo) *)
	{lewy = x; prawy = y; dopelnienie = true}


let mn_el a b =																			(* zwraca a * b z zalozeniem, ze: 0 * +/-oo = 0 *)
	if (a = 0. && abs_float b = infinity) || ( abs_float a = infinity && b = 0.) then
		0.
	else
		a *. b


let mnozenie a b =																		(* zwraca [a;b] * [c;d] = [max{a*c,a*d,b*c,b*d};min{...}] *)																				
	{lewy = min4 (mn_el a.lewy b.lewy) (mn_el a.lewy b.prawy) (mn_el a.prawy b.lewy) (mn_el a.prawy b.prawy);
	prawy = max4 (mn_el a.lewy b.lewy) (mn_el a.lewy b.prawy) (mn_el a.prawy b.lewy) (mn_el a.prawy b.prawy);
	dopelnienie = false}


let pol_dop_l w =																		(* zwraca (-oo;a] *)
	{lewy = neg_infinity; prawy = w.lewy; dopelnienie = false}


let pol_dop_p w =																		(* zwraca [b;+oo) *)
	{lewy = w.prawy; prawy = infinity; dopelnienie = false}


let rec polacz a b =																	(* zwraca sume a U b *)	
	if a.dopelnienie = true && b.dopelnienie = true then								(* a i b to dopelnienia to wykonujemy polacz na ich polowkach *)
		polacz (polacz (pol_dop_l a) (pol_dop_l b)) (polacz (pol_dop_p a) (pol_dop_p b))
	else if a.lewy = neg_infinity && b.prawy = infinity then							(* a i b w formie: (-oo;a.p], [b.l;+oo): *)
		if a.prawy >= b.lewy then														(* 	jesli a.p >= b.l wtedy (-oo;+oo) *)
			moja_niesk
		else																			(* 	jesli nie to dopelnienie *)
			wartosc_od_do_dop a.prawy b.lewy
	else if a.lewy = neg_infinity && b.lewy = neg_infinity then 						(* a i b w formie (-oo;a.p], (-oo;b.p] wtedy (-oo;max{a,b}] *)
		{lewy = neg_infinity; prawy = max a.prawy b.prawy; dopelnienie = false}
	else if a.prawy = infinity && b.prawy = infinity then 								(* a i b w formie [a.l;+oo), [b.l;+oo) wtedy [min{a,b};+oo) *)
		{lewy = min a.lewy b.lewy; prawy = infinity; dopelnienie = false}
	else if b.prawy >= a.lewy then														(* a i b w formie [a.l;+oo), [-oo;b.p] wtedy 1. war na odwr. *)
		moja_niesk
	else 
		wartosc_od_do_dop b.prawy a.lewy
		


let mn_dop1 a b =																		(* zwraca [a.l;a.p] * (-oo;b.l] U [b.p;+oo) *)
	polacz (mnozenie a (pol_dop_l b)) (mnozenie a (pol_dop_p b))


let odwrotnosc w b =																	(* zwraca elemnet odwrotny do w: *)
	if b then																			(* jesli w zawiera 0: *)
		if czy_nies_spoj w then															(* 		jesli w = (-oo;+oo) wtedy (-oo;+oo) *)
			moja_niesk
		else if w.dopelnienie = true then													
			if w.lewy = 0. then															(* 		jesli w = (-oo;0] U [a;+oo) wtedy (-oo; 1/a] *)
				{lewy = neg_infinity; prawy = 1. /. w.prawy; dopelnienie = false}
			else if w.prawy = 0. then 													(* 		jesli w = (-oo;a] U [0;+oo) wtedy [1/a;+oo) *)
				{lewy = 1. /. w.lewy; prawy = infinity; dopelnienie = false}	
			else																		(* 		jesli w = (-oo;a] U [b;+oo) wtedy (-oo;+oo) *)
				wartosc_od_do_dop (1. /. w.prawy) (1. /. w.lewy)
		else if w.lewy = 0. then														(* 		jesli w = [0;a] wtedy [1/a;+oo) *)
				{lewy = 1. /. w.prawy; prawy = infinity; dopelnienie = false}	
			else if w.prawy = 0. then													(*		jesli w = [a;0] wtedy (-oo;1/a] *)
				{lewy = neg_infinity; prawy = 1. /. w.lewy; dopelnienie = false}
			else																		(* 		jesli w = [a;b] wtedy (-oo;1/a] U [1/b;+oo) *)
				wartosc_od_do_dop (1. /. w.lewy) (1. /. w.prawy)
	else																				(* jesli w nie zawiera 0 to [min{1/a,1/b};max{1/a,1/b}] *)												
		{lewy = min (1. /. w.lewy) (1. /. w.prawy); 
		prawy = max (1. /. w.prawy) (1. /. w.lewy); dopelnienie = false}

(*****************************************************************************************************************************************************)
(* konstruktory *)

let wartosc_od_do x y = 																(* tworzy [x;y] *)
	{lewy = abs_zero x; prawy = abs_zero y; dopelnienie = false}

let wartosc_dokladnosc x p = 															(* tworzy [x-p%;x+p%] *)
	wartosc_od_do (x -. abs_float (x *. p /. 100.)) (x +. abs_float (x *. p /. 100.))

let wartosc_dokladna x =																(* tworzy [x;x]*)
	wartosc_od_do x x

(*****************************************************************************************************************************************************)
(* selektory *)

let in_wartosc w x =
	if jest_nan w then 																	(* do nan nic nie nalezy *)
		false
	else if w.dopelnienie = false then													(* nalezenie do [a;b] *)
		(x >= w.lewy && x <= w.prawy)
	else 
		(x <= w.lewy || x >= w.prawy)													(* nalezenie do (-oo;a] U [b;+oo) *)


let min_wartosc w =																		
	if jest_nan w then 																	(* min war z nan to nan *)
		nan
	else if w.dopelnienie = false then													(* min war z [a;b] to a *)
		w.lewy
	else																				(* min war z (-oo;a] U [b,+oo) to -oo *)
		neg_infinity	


let max_wartosc w =
	if jest_nan w then 																	(* max war z nan to nan *)
		nan
	else if w.dopelnienie = false then 													(* max war z [a;b] to b *)
		w.prawy
	else																				(* max war z (-oo;a] U [b;+oo) to +oo *)
		infinity


let sr_wartosc w =
	if jest_nan w then																	(* sr wartosc dla nan to nan *)
		nan
	else if ((min_wartosc w = neg_infinity) && (max_wartosc w = infinity)) then 		(* sr wartosc dla (-oo;+oo) to nan *)
		nan
	else																				(* w innych wypadkach sr wartosc dla [a;b] to (a+b)/2 *)
		((min_wartosc w +. max_wartosc w) /. 2.)

(*****************************************************************************************************************************************************)
(* modyfikatory *)

let plus a b =
	if jest_nan a || jest_nan b then													(* nan + x = nan *)
		moj_nan									
	else if a.dopelnienie = true && b.dopelnienie = true then 							(* (-oo;a] U [b;+oo) + (-oo;c] U [d;+oo) = (-oo;+oo) *)
		moja_niesk					
	else if a.dopelnienie = true then													(* (-oo;a] U [b;+oo) + [c;d] = (-oo;a+d] U [b+c;+oo) *)
		if (a.lewy +. b.prawy) >= (a.prawy +. b.lewy ) then								(* chyba ze a+d ≥ b+c wtedy (-oo;+oo) *)
			moja_niesk
		else											
			wartosc_od_do_dop (a.lewy +. b.prawy) (a.prawy +. b.lewy)
	else if b.dopelnienie = true then													(* [a;b] + (-oo;c] U [d;+oo) = (-oo;b+c] U [a+d;+oo) *)
		if (a.prawy +. b.lewy) >= (a.lewy +. b.prawy) then								(* chyba ze b+c ≥ a+d wtedy (-oo;+oo) *)
			moja_niesk
		else
			wartosc_od_do_dop (a.prawy +. b.lewy) (a.lewy +. b.prawy)
	else																				(* [a;b] + [c;d] = [a+c;b+d] *)
		wartosc_od_do (a.lewy +. b.lewy) (a.prawy +. b.prawy)	


let minus a b = 																		(* A - B = A + (-B) *)
	plus a (negacja b)																	


let razy a b =
	if jest_nan a || jest_nan b then													(* a = nan v b = nan to a * b = nan *)
		moj_nan
	else if czy_zero a || czy_zero b then												(* 0 * x = y * 0 = 0 *)
		{lewy = 0.; prawy = 0.; dopelnienie = false}
	else if a.dopelnienie = true && b.dopelnienie = true then							(* (-oo;a] U [b,+oo) * (-oo;c] U [d;+oo) =    *)
		polacz (mn_dop1 (pol_dop_l a) b) (mn_dop1 (pol_dop_p a) b)						(* ((-oo;a]*(-oo;c] U [d;+oo)) U ([b;+oo)*(-oo;c] U [d;+oo) *)
	else if a.dopelnienie = false && b.dopelnienie = false then							(* [a;b] * [c;d] = [min{a*c,a*d,b*c,b*d}; max{...}] *)
		mnozenie a b
	else if a.dopelnienie = true then													(* (-oo;a] U [b;+oo) * [c;d] = (-oo;a]*[c;d] U [b;+oo)*[c;d] *)
		polacz (mnozenie (pol_dop_l a) b) (mnozenie (pol_dop_p a) b)
	else 																				(* [a;b] * (-oo;c] U [d;+oo) = [a;b]*(-oo;c] U [a;b]*[d;+oo) *)
		polacz (mnozenie a (pol_dop_l b)) (mnozenie a (pol_dop_p b))


let podzielic a b =
	if jest_nan a || jest_nan b || czy_zero b then										(* x / 0 = nan / y = y / nan = nan *)
		moj_nan
	else 
		razy a (odwrotnosc b (in_wartosc b 0.))											(* x / y = x * 1 / y *)


(*****************************************************************************************************************************************************)
(*testy*)
(*
let a = min_wartosc( podzielic (wartosc_od_do 6. 9.) (wartosc_od_do (-0.) 0. ));; 		(* [6;9] / [0;0] = nan *)
assert (classify_float a = FP_nan);;

let a = razy (wartosc_dokladna 0.) (podzielic (wartosc_dokladna 1.)
	 (wartosc_od_do (-1.) 1.));;														(* [0;0] * (-oo;-1]U[1;+oo) = [0;0] *)
assert (in_wartosc a 0. = true);;
assert (in_wartosc a 1. = false);;
assert (min_wartosc a = 0. );;

let a = in_wartosc ( minus ( wartosc_dokladnosc (1.000000) (9.600000) ) ( podzielic ( wartosc_dokladna (-2.000000) ) ( wartosc_dokladnosc (0.000000) (0.000000) ) ) ) (-5.000000);; 				
assert (a = false);;(* [1;-1] *)

let a = in_wartosc (podzielic (wartosc_dokladnosc 5.4 9.8) (wartosc_dokladna (-6.))) (-5.8);;
assert (a = false);;

let a = in_wartosc ( razy ( wartosc_dokladna (-4.200000) ) ( razy ( wartosc_od_do (-2.200000) (4.800000) ) ( wartosc_dokladnosc (9.800000) (3.400000) ) ) ) (3.800000);;
assert (a = true);;

let a = in_wartosc ( podzielic ( wartosc_dokladna (9.000000) ) ( wartosc_dokladnosc (-5.000000) (9.000000) ) ) (-10.000000);;
assert (a = false);;

let a = in_wartosc ( podzielic ( podzielic ( wartosc_od_do (-10.000000) (-7.000000) ) ( wartosc_od_do (-7.000000) (1.000000) ) ) ( wartosc_dokladna (-6.000000) ) ) (1.000000);;
assert (a = false);;
*)


	