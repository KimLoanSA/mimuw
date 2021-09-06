(* sortowanie topologiczne *)
(* Marcin Abramowicz *)
(* code reviwe: Mateusz Danowski *)

open PMap

(* wyjatek rzucany przez [topol] gdy zaleznosci sa cykliczne *)
exception Cykliczne

(* 3 mozliwe kolory wierzcholkow *)
type kolorek = Bialy | Szary | Czarny

type 'a wierzcholek =
	{
		mutable kolor : kolorek;
		mutable lista : 'a list
	}

(* funkcja tworzaca mape z grafem *)
let rec rob_graf lista wyn =
	match lista with
	| [] -> wyn
	| (wierz, lista)::t ->
		rob_graf t (add wierz {kolor = Bialy; lista = lista} wyn)

let topol l =
	let graf = rob_graf l empty in
	let rec dfs wierz wyn = 
		let wierz_map = find wierz graf in
		if wierz_map.kolor = Szary then
			raise Cykliczne
		else
			let f acc x =
				let pom = find x graf in
				if pom.kolor <> Czarny then dfs x acc
				else acc in

			wierz_map.kolor <- Szary;
			let akt = List.fold_left f wyn wierz_map.lista in
			wierz_map.kolor <- Czarny;
			wierz::akt in	
	let wyn = ref [] in
	let rec przejdz l =
		match l with
		| [] -> ()
		| (wierz, _)::t ->
			let pom = find wierz graf in
			if pom.kolor = Bialy then
				wyn := dfs wierz !wyn;
				przejdz t
			else
				przejdz t in

	przejdz l;
	!wyn
;;

