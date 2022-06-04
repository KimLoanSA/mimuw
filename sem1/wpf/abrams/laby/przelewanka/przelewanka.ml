(* przelewanka *)
(* Marcin Abramowicz *)
(* code review: Mateusz Danowski *)

(* mozna uzyskac koncowy stan jesli: *)
(* -nwd pojemnosci dzieli kazda oczekiwana ilosc wody w szklankach *)
(* -jest przynajmniej jedna oczekiwana pusta/pelna szklanka *)

(* wynik otrzymyjemu puszczajac bfs po wszystkich mozliwych stanach, *)
(* zapewnia nam to, ze jesli osiagniemy dany stan to osiagniemy go w najmniejszej liczbie operacji *)
(* zeby dany stan przetwarzac maksymalnie raz zaznaczamy jakie stany juz osiagnelismy *)
(*******************************************************************************************************************************************************************************************)
(* funkcje pomocnicze *)

let rec nwd a b = (* funkcja liczaca najwiekszy wspolny dzielnik a i b *)
	if a = 0 then b
	else nwd (b mod a) a


let sprawdz_poj szklanki n = (* funkcja sprawdzajaca czy jest pusta/pelna szklanka - wpp odpowiedz to -1 *)
	let wyn = ref false in

	for i = 0 to n - 1 do 
		if (snd szklanki.(i) = 0) || (snd szklanki.(i) = fst szklanki.(i)) then
			wyn := true;
	done;
	!wyn


let sprawdz_nwd szklanki n = (* funkcja sprawdzajaca czy oczekiwana ilosc wody w szklankach dzieli sie przez nwd pojemnosci szklanek - wpp odpowiedz to -1 *)
	let nwd_pojemnosc = ref (fst szklanki.(0)) 
	and wyn = ref true in

	for i = 0 to n - 1 do 
		nwd_pojemnosc := nwd !nwd_pojemnosc (fst szklanki.(i));
	done;

	for i = 0 to n - 1 do 
		if snd szklanki.(i) mod !nwd_pojemnosc > 0 then
			wyn := false;
	done;	
	!wyn


let sprawdz_czy_mozna szklanki n = (* funkcja sprawdzajaca czy da sie odpowiednio poprzelewac wode *)
	(sprawdz_nwd szklanki n) && (sprawdz_poj szklanki n)


let usun_0 szklanki = (* funkcja usuwajaca z tablicy szklanki o pojemnosci 0 *)
	let n = Array.length szklanki in
	let wielkosc = ref 0 in

	for i = 0 to n - 1 do
		if fst szklanki.(i) > 0 then
			incr wielkosc;
	done;

	let wyn = Array.make !wielkosc (0,0) 
	and indeks = ref 0 in

	for i = 0 to n - 1 do
		if fst szklanki.(i) > 0 then begin
			wyn.(!indeks) <- szklanki.(i);
			incr indeks;
		end;
	done;
	wyn

(*******************************************************************************************************************************************************************************************)
(* funkcja *)

let przelewanka szklanki = (* zgodnie z trescia zadania *)
	let szklanki = usun_0 szklanki in (* usuwamy z tablicy szklanki o pojemnosci 0 bo nie zmieniaja wyniku *)
	let n = Array.length szklanki in 

	if n = 0 then 0 (* jesli n = 0 to wynik = 0 *)
	else if not (sprawdz_czy_mozna szklanki n) then -1 (* sprawdzamy czy da sie uzyskac wynik *)
	else begin
		let stan_start = Array.make n 0 (* tablica ze stanem poczatkowym - wszystkie szklanki puste *)
		and stan_koniec = Array.make n 0 
		and mapa = Hashtbl.create 100007 (* hash mapa przechowujaca odwiedzone stany *)
		and kolejka = Queue.create () 
		and czy_szukac = ref true in begin

		for i = 0 to n - 1 do (* ustalamy stan koncowy zgodnie z wejsciem *)
			stan_koniec.(i) <- snd szklanki.(i);
		done;

		let oproznij stan kroki = (* funckja oprozniajaca kazda niepusta szklanke *)
			for i = 0 to n - 1 do 
				let pom = Array.copy stan in

				if stan.(i) > 0 then begin
					pom.(i) <- 0;

					if Hashtbl.mem mapa pom = false then
						Queue.push (pom, kroki + 1) kolejka;
				end;
			done


		and nalej stan kroki = (* funkcja napelniajaca kazda niepelna szklanke *)
			for i = 0 to n - 1 do 
				let pom = Array.copy stan in

				if stan.(i) < fst szklanki.(i) then begin
					pom.(i) <- fst szklanki.(i);

					if Hashtbl.mem mapa pom = false then 	
						Queue.push (pom, kroki + 1) kolejka;
				end;
			done


		and przelej stan kroki indeks = (* funkcja przelewajaca wode z i-tej szklanki do wszystkich pozostalych *)
			let akt = stan.(indeks) in

			for i = 0 to n - 1 do 
				let pom = Array.copy stan in

				if stan.(i) < fst szklanki.(i) && i <> indeks then begin (* tylko czesc wody ze szklanki zmiesci sie w i-tej *)
					if akt + stan.(i) > fst szklanki.(i) then begin
						pom.(i) <- fst szklanki.(i);
						pom.(indeks) <- akt - (fst szklanki.(i)) + stan.(i);
					end else begin (* cala woda ze szklanki zmiesci sie w i-tej *)
						pom.(i) <- stan.(i) + akt;
						pom.(indeks) <- 0;
					end;

					if Hashtbl.mem mapa pom = false then
						Queue.push (pom, kroki + 1) kolejka;
				end;
			done in


		let przelej_wszystkie stan kroki = (* funkcja przelewajaca wode miedzy kazda para szklanek *)
			for i = 0 to n - 1 do
				if stan.(i) > 0 then
					przelej stan kroki i;
			done in

			Queue.push (stan_start, 0) kolejka; (* inicjujemy kolejke ze stanem startowym *)

			while not (Queue.is_empty kolejka) && !czy_szukac do (* szukamy az nie dojdziemy do stanu koncowego *)
				let (stan, kroki) = Queue.pop kolejka in (* wyjmujemy z kolejki aktualny stan *)

				if Hashtbl.mem mapa stan = false then begin (* wykonujemy kroki jesli nie bylismy jeszcze w danym stanie *)
					Hashtbl.add mapa stan kroki;

					if stan = stan_koniec then (* jesli doszlismy do stanu koncowego zaznaczamy, ze nie trzeba juz szukac *)
						czy_szukac := false;

					nalej stan kroki; (* napelniamy kazda szklanke *)
					oproznij stan kroki; (* oprozniamy kazda szkalnke *)
					przelej_wszystkie stan kroki; (* przelewamy miedzy szklankami *)
				end;
			done;
		end;
		Hashtbl.find mapa stan_koniec; (* zwracamy wynik dla stanu koncowego *)
	end


(*******************************************************************************************************************************************************************************************)
(* testy *)

(*
let test a answ =
	przelewanka a = answ;;


let a = [|(1, 1); (2, 1)|];;
assert(test a 2);;

let a = [||];;
assert(test a 0);;

let a = [|(10, 5); (4, 3); (3, 2); (2, 0)|];;
assert(test a 5);;

let a = [|(50, 50); (50, 48); (2, 2)|];;
assert(test a 3);;

let a = [|(50, 50); (50, 47); (2, 2)|];;
assert(test a (-1));;

let a = [|(13, 9); (17, 3); (7, 2); (2, 2)|];;
assert(test a 9);;

let a = [|(1, 0); (1000000, 999999)|];;
assert(test a 3);;

let a = [|(1, 0); (1000000, 999997)|];;
assert(test a 7);;

let a = [|(9, 6); (12, 9); (12, 3); (999, 411)|];;
assert(test a (-1));;

let a = [|(37, 35); (55, 36)|];;
assert(test a (-1));;

let a = [|(2, 1); (0, 0); (4, 2);|];;
assert(test a (-1));;

let a = [|(0, 0); (0, 0); (0, 0); (0, 0); (0, 0); (0, 0); (99, 66); (3, 3)|];;
assert(test a 22);;

let a = [|(37, 3); (42, 37); (69, 33)|];;
assert(test a (-1));;

let a = [|(1, 0); (1000, 999); (1000000, 999999); (1000000000000, 999999999999)|];;
assert(test a 9);;

let a = [|(24, 13); (12, 5); (6, 2); (1, 0)|];;
assert(test a 10);;

let a = [|(100, 0); (50, 0); (100000, 0); (35, 0)|];;
assert(test a 0);;



assert (przelewanka [| (10,2); (1,1) |] = 5);;
assert (przelewanka [| (0,0); (2,2); (2,2); (2,2); (0,0); (0,0); (1,0);
  (0,0); (1,0) |] = (3));;
assert (przelewanka [| (1,1); (2,1); (3,0); (4,2) |] = (3));;
assert (przelewanka [| (0,0); (2,2); (1,0); (1,1); (1,0); (2,2); (1,0);
  (0,0); (0,0) |] = (3));;
assert (przelewanka [| (11,11); (11,1) |] = (-1));;
assert (przelewanka [| (1,1); (0,0); (2,2); (0,0); (2,0); (0,0); (0,0);
  (1,0); (2,0); (1,0) |] = (2));;
assert (przelewanka [| (5,2); (0,0); (0,0); (2,0); (3,2) |] = (4));;
assert (przelewanka [| (1,1); (0,0); (4,4); (4,0); (4,4) |] = (3));;
assert (przelewanka [| (9,9); (13,12) |] = (10));;
assert (przelewanka [| (2,2); (1,0); (2,2); (0,0); (1,0); (0,0); (1,1);
  (1,0); (0,0) |] = (3));;
assert (przelewanka [| (5,2); (3,1); (0,0); (4,1); (0,0); (1,0) |] = (5));;
assert (przelewanka [| (310,76); (139,91) |] = (-1));;
assert (przelewanka [| (48,9); (12,0); (1,1); (65,64) |] = (10));;
assert (przelewanka [| (7,5); (3,3); (9,4); (10,4); (6,3); (5,3) |] =
  (8));;
assert (przelewanka [| (100000,50000); (1,1) |] = (100000));;
assert (przelewanka [| (0,0); (0,0); (0,0); (300000,151515);
  (1,0); (0,0) |] = (296971));;
assert (przelewanka [| (11,2); (11,10); (4,0); (10,8); (21,16) |] = (12));;
assert (przelewanka [| (50,1); (7,3); (78,64) |] = (-1));;
assert (przelewanka [| (85,23); (524,210) |] = (-1));;
assert (przelewanka [| (557,349); (73,49) |] = (-1));;
assert (przelewanka [| (62,3); (38,7) |] = (-1));;
assert (przelewanka [| (15,15); (6,3); (42,32); (33,20) |] = (-1));;
assert (przelewanka [| (39,12); (35,34); (21,7); (2,1) |] = (-1));;
assert (przelewanka [| (1,0); (2,1); (2,1); (0,0); (2,0); (0,0); (0,0);
  (0,0); (1,1); (0,0); (1,0) |] = (4));;
assert (przelewanka [| (2,0); (2,2); (2,1); (6,6); (0,0) |] = (-1));;
assert (przelewanka [| (2,0); (1,1); (1,1); (1,1); (0,0); (1,0); (3,2);
  (0,0) |] = (4));;
assert (przelewanka [| (1,1); (2,2); (4,1); (0,0); (1,0); (2,1) |] = (5));;
assert (przelewanka [| (1,0); (3,1); (2,2); (1,1); (1,0); (1,0) |] = (3));;
assert (przelewanka [| (20,7); (12,11) |] = (-1));;
assert (przelewanka [| (0,0); (21,21) |] = (1));;
assert (przelewanka [| (13,8); (11,11) |] = (14));;
assert (przelewanka [| (1,1); (3,2); (6,5) |] = (5));;
assert (przelewanka [| (4,4); (7,6); (2,2) |] = (6));;
assert (przelewanka [| (3,2); (3,3); (1,1); (2,0) |] = (3));;
assert (przelewanka [| (0,0); (2,0); (0,0); (2,0); (3,2); (2,1); (1,0) |] =
  (3));;

 *)