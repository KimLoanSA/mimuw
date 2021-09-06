(* drzewa lewicowe *)
(* Marcin Abramowicz *)
(* code review: Michał Niedziółka *)

type 'a queue =		(* reprezentacja drzewa *)	
	| Node of 'a queue * 'a queue * 'a * int	(* lewe poddrzewo, prawe poddrzewo, wartosc, glebokosc prawego poddrzewa *)
	| Null	(* puste drzewo *)

exception Empty 	(* wyjatek Empty *)

let empty =		(* zwraca pusta kolejke *) 
	Null

let height q =	(* zwraca wysokosc *)
	match q with
	| Null -> 0
	| Node (_, _, _, d) -> d

let is_empty q =	(* sprawdza czy kolejka jest pusta *)
	match q with 
	| Null -> true
	| _ -> false


let make_node left right value height =		(* zwraca wierzcholek *)
	Node (left, right, value, height)


let make_empty_node a =		(* zwraca pusty wierzcholek *)
	make_node empty empty a 1


let rec join a b =		(* laczy dwa drzewa *)
	match a, b with
	| Null, _ -> b 		(* jesli 1. pusta zwracamy 2. *)
	| _, Null -> a 		(* jesli 2. pusta zwracamy 1. *)
	| Node (left_a, right_a, value_a, height_a), Node (left_b, right_b, value_b, height_b) ->
		if value_b < value_a then		(* jesli najmniejsza wartosc w 2. kolejce jest mniejsza od wartosci w 1. to zamieniamy kolejnosc *)
			join b a
		else let c = join right_a b 		(* laczymy prawe poddrzewo 1. drzewa i 2. drzewo *)
		in if height left_a < height c then				(* jesli wysokosc lewego poddrzewa 1. drzewa jest mniejsza niz wysokosc polaczonych to *)
			make_node c left_a value_a (height_a + 1)	(* to lewym poddrzewem zostaja polaczone poddrzewa, a prawym lewe poddrzewo drzewa 1. *)
		else
			make_node left_a c value_a (height c + 1)	(* w przeciwnym wypadku powstaje wierzcholek odwrotny *)

let delete_min q =		(* zwraca najmniejsza wartosc w drzewie, jesli nie istnieje to podnosimy wyjatek *)
	match q with
	| Null -> raise Empty
	| Node (left, right, value, height) -> (value, join left right)


let add a q =		(* dodaje wartosc a do drzewa *)
	join (make_empty_node a) q


(* testy *)
(*
let a0 = empty;;
assert (is_empty a0);;
let a1 = empty;;
let a2 = add 997 a0;;
let a3 = add 94 a0;;
let (v, a4) = delete_min a2;;
assert (v == 997);;
let a5 = join a1 a3;;
assert (is_empty a4);;
let a6 = empty;;
let a7 = add 880 a3;;
assert (is_empty a0);;
let a8 = empty;;
let a9 = add 102 a3;;
let a10 = join a0 a7;;
let a11 = add 604 a0;;
assert (is_empty a6);;
let a12 = empty;;
let (v, a13) = delete_min a11;;
assert (v == 604);;
let (v, a14) = delete_min a2;;
assert (v == 997);;
let a15 = add 229 a11;;
let (v, a16) = delete_min a15;;
assert (v == 229);;
let (v, a17) = delete_min a11;;
assert (v == 604);;
let (v, a18) = delete_min a16;;
assert (v == 604);;
let a19 = add 62 a10;;
assert (is_empty a0);;
let a20 = empty;;
let a21 = join a11 a15;;
let (v, a22) = delete_min a5;;
assert (v == 94);;
assert (is_empty a8);;
let a23 = empty;;
let a24 = empty;;
let a25 = join a12 a21;;
let a26 = add 509 a19;;
let a27 = add 345 a17;;
let (v, a28) = delete_min a15;;
assert (v == 229);;
let (v, a29) = delete_min a28;;
assert (v == 604);;



let a = empty;;
let b = add 1 empty;;

assert (is_empty a = true);;
assert (try let _=delete_min a in false with Empty -> true);;
assert (is_empty b <> true);;

let b = join a b ;;
assert (is_empty b <> true);;

let (x,y) = delete_min b;;

assert (x = 1);;
assert (is_empty y = true);;
assert (try let _=delete_min y in false with Empty -> true);;

let b = add 1 empty;;
let b = add 3 b;;
let b = add (-1) b;;
let b = add 2 b;;
let b = add 1 b;;

let (a,b) = delete_min b;;
assert (a = -1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 2);;

let (a,b) = delete_min b;;
assert (a = 3);;

assert(is_empty b = true);;

let b = add "a" empty;;
let b = add "aca" b;;
let b = add "nzbzad" b;;
let b = add "nzbza" b;;
let b = add "bxbxc" b;;

let (a,b) = delete_min b;;
assert (a = "a");;

let (a,b) = delete_min b;;
assert (a = "aca");;

let (a,b) = delete_min b;;
assert (a = "bxbxc");;

let (a,b) = delete_min b;;
assert (a = "nzbza");;

let (a,b) = delete_min b;;
assert (a = "nzbzad");;

assert(is_empty b = true);;
assert (try let _=delete_min b in false with Empty -> true);;
*)