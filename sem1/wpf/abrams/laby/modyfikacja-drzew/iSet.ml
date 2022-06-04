(* modyfikacja drzew *)
(* Marcin Abramowicz *)
(* code review: Konrad Czaplinski *)

(*****************************************************************************************************************************************************************************************)
(* reprezentacja *)

type 'k set = (* reprezentacja drzewa *)
  | Empty (* puste *)
  | Node of 'k set * 'k * 'k set * int * int (* wierzcholek: (1)lewe poddrzewo, (2)wartosc w wierzcholku, (3)prawe poddrzewo, (4)glebokosc poddrzewa, (5)liczba elementow*)
(* liczba elementow w znaczeniu liczby liczb calkowitych, ktore zawieraja sie w przedzialach *)

type t =
  {
    cmp : (int * int) -> (int * int) -> int; (* comparator *)
    set : (int * int) set; (* 'k = para intow *)
  }

(*****************************************************************************************************************************************************************************************)
(* funkcje pomocnicze *)

let height a = (* funkcja zwracajaca wysokosc poddrzewa *)
	match a with
	| Node (_, _, _, h, _) -> h
	| Empty -> 0

let size a = (* funkcja zwracajaca liczbe elemntow w poddrzwie *)
	match a with
	| Node (_, _, _, _, s) -> s
	| Empty -> 0

let length (l, r) = (* funkcja zwracajaca liczbe elementow w przedziale *)
  r - l + 1

let sum_length l k r = (* funkcja sumujaca wielkosci poddrzew *)
  if size l < 0 || size r < 0 || length k <= 0 then -1(* jesli ktores przekracza max_int to i wynik bedzie przekraczal *)
  else 
    let sum = size l + size r + length k in 
      if sum < 0 then -1 (* jesli po zsumowaniu przekracza max_int wtedy zaznaczamy, ze przekracza *)
      else sum

let make l k r = (* funkcja zwracajaca wierzcholek o zadanych poddrzewach i elemencie *)
	Node (l, k, r, max (height l) (height r) + 1, sum_length l k r)

(* na wejsciu dajemy niezbalansowane drzewo gdzie roznica wysokosci poddrzew jest <= 3 *)
(* funkcja zwraca zbalansowane drzewo, czyli takie gdzie roznica wysokosci poddzew <= 1 *)
let bal l k r = (* funkcja balansujaca drzewo *)
	let hl = height l in
	let hr = height r in
	if hl > hr + 2 then
		match l with
		| Node (ll, lk, lr, _, _) ->
      if height ll >= height lr then 
        make ll lk (make lr k r)
      else
        (match lr with
        | Node (lrl, lrk, lrr, _, _) ->
          make (make ll lk lrl) lrk (make lrr k r)
        | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
      if height rr >= height rl then 
        make (make l k rl) rk rr
      else
        (match rl with
        | Node (rll, rlk, rlr, _, _) ->
          make (make l k rll) rlk (make rlr rk rr)
        | Empty -> assert false)
    | Empty -> assert false
  else 
  	make l k r

let rec min_elt = function (* funkcja zwracajaca minimalny element *)
	| Node (Empty, k, _, _, _) -> k
	| Node (l, _, _, _, _) -> min_elt l
	| Empty -> raise Not_found

let rec remove_min_elt = function (* funkcja usuwajaca element minimalny *)
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt"

let create cmp = (* funkcja tworzacza set z danym komparatorem *)
	{cmp = cmp; set = Empty}


let rec add_one cmp x = function (* funkcja dodajaca przedzial do drzewa, przedzial ten sie nie przecina z zadnym z drzewa, a drzewo jest zbalansowane *)
  | Node (l, k, r, h, s) ->
    let c = cmp x k in
    if c = 0 then 
      Node (l, x, r, h, s)
    else if c < 0 then
      let nl = add_one cmp x l in
      bal nl k r
    else
      let nr = add_one cmp x r in
      bal l k nr
  | Empty -> Node (Empty, x, Empty, 1, sum_length Empty x Empty)


let rec join cmp l v r = (* funkcja laczaca dwa poddrzewa, zwraca zbalansowane drzewo *)
  match (l, r) with
  | (Empty, _) -> add_one cmp v r
  | (_, Empty) -> add_one cmp v l
  | (Node(ll, lv, lr, lh, ls), Node(rl, rv, rr, rh, rs)) ->
    if lh > rh + 2 then 
      bal ll lv (join cmp lr v r) 
    else if rh > lh + 2 then 
      bal (join cmp l v rl) rv rr 
    else
      make l v r

let merge_with_join {cmp = cmp1; set = t1} {cmp = cmp2; set = t2} = (* funkcja laczaca dwa drzewa z uzyciem join, drzewa na poczatku nie musza miec podobnych wysokosci, ale zwraca zrownowazone *)
  match t1, t2 with
    | Empty, _ -> t2
    | _, Empty -> t1
    | _ ->
      let k = min_elt t2 in
      join cmp1 t1 k (remove_min_elt t2)

let inclusion x (l, r) = (* funkcja sprawdza czy element x zawiera sie w przedziale [l;r] *)
	(x <= r) && (x >= l)

let add_one_help l r {cmp = cmp; set = set} = (* funkcja dodajaca element do seta, ale sprawdzajaca czy istnieje taka potrzeba *)
  if l > r then (* jesli lewy koniec przedzialu przekracza prawy nic nie dodajemy i zwracamy wejsciowe drzewo *)
		set
	else (* w przeciwnym wypadku dodajemy przedzial *)
		add_one cmp (l,r) set 

let crossing (al, ar) (bl, br) = (* funkcja sprawdzajace przecinanie sie 2 przedzialow, gdzie przedzialy [a;b] i [b+1;c] tez sie przecinaja *)
	if ar = max_int && br = max_int then
		true
	else if ar = max_int && br <> max_int then
		(ar >= bl && al <= br) || (br + 1 >= al && bl <= ar)
	else if br = max_int && ar <> max_int then
		(br >= al && bl <= ar) || (ar + 1 >= bl && al <= br)
	else
		(ar + 1 >= bl && al <= br) || (br + 1 >= al && bl <= ar)

(* na wejsciu jest przedzial, zrownowazone drzewo i akumulator *)
let rec minimal_crossing x {cmp = cmp; set = set} acc = (* funkcja zwracajaca minimalny przedzial przecinajacy sie z danym *)
	match set with
	| Empty -> acc (* jesli jestesmy w lisciu zwracamy akumulator *)
	| Node (l, (vl, vr), r, _, _) ->
		if crossing x (vl, vr) then (* jesli przedzial sie przecina z danym to porawiamy akumulator i idziemy w lewo *)
			minimal_crossing x {cmp = cmp; set = l} (vl, vr) 
		else if cmp x (vl, vr) < 0 then (* jesli jestesmy mniejsi od wezla idziemy w lewo *)
			minimal_crossing x {cmp = cmp; set = l} acc
		else (* wpp idziemy w prawo *)
			minimal_crossing x {cmp = cmp; set = r} acc

(* na wejsciu jest przedzial, zrownowazone drzewo i akumulator *)
let rec maximal_crossing x {cmp = cmp; set = set} acc = (* funkcja zwracajaca maksymalny przedzial przecinajacy sie z danym *)
	match set with
	| Empty -> acc (* jesli jestesmy w lisciu zwracamy akumulator *)
	| Node (l, (vl, vr), r, _, _) ->
		if crossing x (vl, vr) then (* jesli przedzial sie przecina z danym to porawiamy akumulator i idziemy w prawo *)
			maximal_crossing x {cmp = cmp; set = r} (vl, vr)
		else if cmp x (vl, vr) < 0 then (* jesli jestesmy mniejsi od wezla idziemy w lewo *)
			maximal_crossing x {cmp = cmp; set = l} acc
		else (* wpp idziemy w prawo *)
			maximal_crossing x {cmp = cmp; set = r} acc

(*****************************************************************************************************************************************************************************************)
(* funkcje *)

let empty = (* pusty set *)
	{cmp = compare; set = Empty}

let is_empty x = (* funkcja sprawdzajaca czy set jest pusty *)
  x.set = Empty

(* na wejsciu zbalansowane drzewo, zwraca zbalnsowane poddrzewa spelniajace dany warunek *)
let split x {cmp = cmp; set = set} = (* funkcja dzielaca drzewo wedlug podanego elementu *)
  let rec loop x = function
    | Empty -> (Empty, false, Empty)
    | Node (l, (vl, vr), r, _, _) -> 
      if inclusion x (vl, vr) then (* jesli element znajduje sie w jakims przedziale, to znalezlismy *)
        if x = max_int then (* jesli element jest rowny max int to znaczy ze nie musimy nic dodawac po jego prawej stronie *)
          (add_one_help vl (x - 1) {cmp = cmp; set = l}, true, r)
        else if x = min_int then (* jesli element jest rowny min int to znaczy ze nie musimy nic dodawac po jego lewej stronie *)
          (l, true, add_one_help (x + 1) vr {cmp = cmp; set = r})
        else (* wpp dodajemy do lewego i prawego poddrzewa odpowiednio przedzialy [a;x-1] i [x+1;b], gdzie przedzial [a;b] to przedzial, ktorym zawiera sie x (dany element) *)
            (add_one_help vl (x - 1) {cmp = cmp; set = l}, true, add_one_help (x + 1) vr {cmp = cmp; set = r}) (* funkcja add_one_help sprawdza czy jest potrzeba dodawania *)
      else if x < vl then (* jesli element jest mniejszy niz aktualny przedzial idziemy w lewo *)
        let (ll, pres, rl) = loop x l in 
        (ll, pres, join cmp rl (vl, vr) r)
      else (* wpp idziemy w prawo *)
        let (lr, pres, rr) = loop x r in 
        (join cmp l (vl, vr) lr, pres, rr)
  in
    let setl, pres, setr = loop x set in
    {cmp = cmp; set = setl}, pres, {cmp = cmp; set = setr}
  
(* na wejsciu zbalansowane drzewo, na wyjsciu rozwiez *)
let remove (xl, xr) {cmp = cmp; set = set} = (* funkcja usuwajaca wszystkie elementy z przedzialu *)
  let (left, _, _) = split xl {cmp = cmp; set = set} (* bierzemy lewe poddrzewo na lewego od lewego konca zadanego przedzialu *)
  and (_, _, right) = split xr {cmp = cmp; set = set} (* bierzemy prawe poddrzewo na prawo od prawego konca zadanego przedzialu*) in
  {cmp = cmp; set = merge_with_join left right}

(* na wejsciu zbalansowane drzewo, na wyjsciu rowniez *)
let add (xl, xr) {cmp = cmp; set = set} = (* funkcja dodajaca element x do seta *)
	let left = minimal_crossing (xl, xr) {cmp = cmp; set = set} (xl, xr) (* sprawdzamy jaki najmniejszy przedzial przecina sie z zadanym *)
	and right = maximal_crossing (xl, xr) {cmp = cmp; set = set} (xl, xr) (* sprawdzamy jaki najwiekszy przedzial przecina sie z zadanym *) in
    let l = min (fst left) xl (* poczatek przedzialu do usuniecia to minimum z minimalnego i zadanego *)
    and r = max (snd right) xr (* poczatek przedzialu do usuniecia do max z maksymalnego i zadanego *) in
  		let tree = remove (l, r) {cmp = cmp; set = set} (* usuwamy przedzial do usuniecia *) in 
  		{cmp = cmp; set = add_one_help l r tree}

let mem x {cmp = cmp; set = set} = (* funkcja sprawdzajaca czy dany element jest w drzewie *)
  let rec loop = function
    | Node (l, k, r, _, _) ->
      let c = cmp (x, x) k in
      (inclusion x k) || loop (if c < 0 then l else r)
    | Empty -> false 
	in
  	loop set

let iter f {set = set} = (* funkcja nakladajaca funkcje na elementy drzewa *)
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r 
  in
  	loop set

let fold f {cmp = cmp; set = set} acc = (* funkcja fold na drzewie *)
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) -> loop (f k (loop acc l)) r
  in
  	loop acc set

let elements {set = set} = (* funkcja zwracajaca wszystkie przedzialy w drzewie *)
  let rec loop acc = function
    | Empty -> acc
    | Node(l, k, r, _, _) -> loop (k :: loop acc r) l 
  in
  	loop [] set

let below x {cmp = cmp; set = set} = (* funkcja zwracajaca liczbe elementow mniejszych od danego *)
	let (tree, exist, _) = split x {cmp = cmp; set = set} in 
  match tree.set with
	| Empty -> if exist then 1 else 0
	| Node (_, _, _, _, s ) ->
    if s <= 0 || (s + if exist then 1 else 0) < 0 then max_int (* jesli liczba przekracza max_int to zwracamy max_int *)
    else s + if exist then 1 else 0 (* wpp suma poddrzew i jesli dany element istnieje w zbiorze to + 1 *)

(*****************************************************************************************************************************************************************************************)
(* testy *)

(*
let a = add (0, 5) empty;;
let a = add (7, 8) a;;
let a = add (-3, -3) a;;
let a = add (10, 13) a;;
assert(elements a = [(-3, -3); (0, 5); (7, 8); (10, 13)]);;
assert(below 8 a = 9);;
let b = add (6, 6) a;;
let b = remove (6, 6) b;;
let b = add (-100, -5) b;;
let b = add (-4, 6) b;;
assert(elements b = [(-100, 8); (10, 13)]);;
assert(below 10 b = 110);;
let c = remove (2, 10) a;;
assert(elements c = [(-3, -3); (0, 1); (11, 13)]);;
assert(below 12 c = 5);;




let a = empty
let a = add (-20, 5) a
let a = add (6, 18) a
let a = add (4, 10) a
let a = add (14, 16) a
let a = remove (-18, 14) a
let a = remove (5, 17) a;;
assert(mem 14 a = false);;
let a = add (-4, 9) a;;
assert(mem 16 a = false);;
assert(mem (-14) a = false);;
assert(mem 10 a = false);;
let a = remove (-9, 10) a;;
let a = add (-6, 7) a;;
let a = add (-2, 7) a;;
let a = add (-12, 17) a;;
let a = add (-13, 8) a;;
let a = add (-13, -2) a;;
assert(mem 11 a = true);;
assert(elements a = [(-20, -19); (-13, 18)]);;*)